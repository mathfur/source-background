{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (length, lines, break, span)
import Data.Text hiding (concat, map, zip)
import Text.Regex.Posix
import Data.Char
import Data.Maybe
import qualified Data.List as L
import Data.Aeson
import Control.Concurrent (MVar,  newMVar, modifyMVar_)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)
import Control.Applicative

type ServerState = Text

main :: IO ()
main = do
    state <- newMVar ("" :: Text)
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application state rq = do
    WS.acceptRequest rq
    sink <- WS.getSink
    let filename = "client.js"
    ls <- liftIO $ (readFile filename)>>=(return . pack)
    let nodes = getNodes ls
    liftIO $ print nodes
    nodes' <- liftIO $ mapM (updateGrepCount filename) nodes
    let w = Whole nodes' [] 1 57 185
    liftIO $ modifyMVar_ state $ \_ -> do
                            WS.sendSink sink $ WS.textData $ convertToJSON w
                            return ""

data IndexedWord  = IndexedWord Int Text deriving (Show, Eq)
data Node = Node Text Int Int Int deriving (Show, Eq)
data Link = Link Int Int deriving (Show, Eq)
data Whole = Whole [Node] [Link] Int Int Int

-- {
--   "nodes": [{"count": 3, "length": 2, "line": 1, "col": 0}],
--   "links": [],
--   "line_min": 1,
--   "line_max": 57,
--   "window_width": 185
-- }

instance ToJSON Whole where
    toJSON (Whole ns ls line_min line_max win_width) = object [
        "nodes" .= toJSON ns,
        "links" .= toJSON ls,
        "line_min" .= line_min,
        "line_max" .= line_max,
        "window_width" .= win_width
        ]

instance ToJSON Node where
    toJSON (Node w c l grep_count) = object [
                                            "word" .= w,
                                            "length" .= length w,
                                            "line" .= c,
                                            "col" .= l,
                                            "count" .= grep_count
                                            ]

instance ToJSON Link where
    toJSON (Link s t) = object ["source" .= toJSON s, "target" .= toJSON t]

loc :: IndexedWord -> Int
loc (IndexedWord i s) = length s + i

increment :: Int -> IndexedWord -> IndexedWord
increment j (IndexedWord i s) = IndexedWord (i + j) s

convertToJSON :: Whole -> Text
convertToJSON whole = pack $ LC.unpack $ encode $ toJSON whole

getNodes :: Text -> [Node]
getNodes input = L.concat $ map (\(l, line) -> map (getNode l) $ getWords 0 line) $ zip [0..] $ lines input

getNode :: Int -> IndexedWord -> Node
getNode i (IndexedWord j s) = Node s i j 0

updateGrepCount :: FilePath -> Node -> IO Node
updateGrepCount path (Node w c l _) = do
    cnt <- readFile path
    let grep_count' = ((cnt =~ (unpack w)) :: Int)
    return $ (Node w c l grep_count')

getWords :: Int -> Text -> [IndexedWord]
getWords _ "" = []
getWords cur input = map (increment cur) $ fromMaybe [] $ (:) <$> word <*> (getWords <$> (loc <$> word) <*> remainder)
  where
    word = (fst <$> getWord input)
    remainder = (snd <$> getWord input)

getWord :: Text -> Maybe (IndexedWord, Text)
getWord input = if (input' == "") then Nothing else Just (IndexedWord (length r1) r2, r3)
  where
    (r1, input') = break isAlphaNum input
    (r2, r3) = span isAlphaNum input'
