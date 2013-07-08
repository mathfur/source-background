{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (length, lines, break, span, catch)
import Data.Text hiding (concat, map, zip, reverse, null)
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
import Control.Exception (fromException, catch, SomeException)
import Control.Monad (forever)
import Network
import System.IO

type ServerState = Text

main :: IO ()
main = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    server `catch` ex_catch
    putStrLn "Connection closed."
      where
         ex_catch :: SomeException -> IO ()
         ex_catch = (const $ putStrLn "Exception caught.")

server :: IO ()
server = do
    sock <- listenOn (PortNumber 8001)
    state <- newMVar ("" :: Text)
    WS.runServer "0.0.0.0" 9160 $ application sock state
    sClose sock

application :: Socket -> MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application sock state rq = do
    WS.acceptRequest rq
    sink <- WS.getSink
    talk sock sink state

talk :: WS.Protocol p => Socket -> WS.Sink WS.Hybi00 -> MVar ServerState -> WS.WebSockets p ()
talk sock sink state = flip WS.catchWsError catchDisconnect $
  forever $ do
    liftIO $ repeats (receive sock sink)
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \old_text -> do
            WS.sendSink sink $ WS.textData ("disconnected" :: Text)
            return old_text
        _ -> return ()

repeats :: Monad m => m Bool -> m () 
repeats x = x >>= (\x' -> if x' then (return ()) else repeats x)

receive :: Socket -> WS.Sink WS.Hybi00 -> IO Bool
receive sock sink = do
    (h,_,_) <- accept sock
    hSetBuffering h LineBuffering
    editor_state <- (parseEditorState . pack) <$> hGetLine h
    print editor_state
    let filename = editingFile editor_state
    cnt <- liftIO $ (readFile filename)>>=(return . pack)
    do
      let nodes = getNodes cnt
      let nodes' = map (updateGrepCount cnt) nodes
      let w = Whole nodes' [] editor_state
      print w
      WS.sendSink sink $ WS.textData $ convertToJSON w
    do
      hPutStrLn h "HTTP/1.0 200 OK"
      hPutStrLn h "Content-type: text/html"
      hPutStrLn h $ "Content-Length: 0"
      hPutStrLn h ""
    return True

data IndexedWord  = IndexedWord Int Text deriving (Show, Eq)
data Node = Node Text Int Int Int deriving (Show, Eq)
data Link = Link Int Int deriving (Show, Eq)
data Whole = Whole [Node] [Link] EditorState deriving (Show, Eq)
data EditorState = EditorState Int Int Int Int Int FilePath deriving (Show, Eq)

editingFile :: EditorState -> FilePath
editingFile (EditorState _ _ _ _ _ path) = path

instance ToJSON Whole where
    toJSON (Whole ns ls editor_state) = object [
        "nodes" .= toJSON ns,
        "links" .= toJSON ls,
        "editor_state" .= toJSON editor_state
        ]

instance ToJSON EditorState where
    toJSON (EditorState top_line bottom_line win_width col line fname) = object [
        "top_line" .= top_line,
        "bottom_line" .= bottom_line,
        "win_width" .= win_width,
        "col" .= col,
        "line" .= line,
        "fname" .= fname
        ]

instance ToJSON Node where
    toJSON (Node w c l grep_count) = object [
                                            "word" .= w,
                                            "col" .= c,
                                            "line" .= l,
                                            "length" .= length w,
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
getNodes input = L.concat $ map (\(l, line) -> map (getNode l) $ getWords 1 line) $ zip [1..] $ lines input

getNode :: Int -> IndexedWord -> Node
getNode i (IndexedWord j s) = Node s j i 0

updateGrepCount :: Text -> Node -> Node
updateGrepCount cnt (Node w c l _) = Node w c l grep_count'
    where
      grep_count' = (((unpack cnt) =~ (unpack w)) :: Int)

getWords :: Int -> Text -> [IndexedWord]
getWords _ "" = []
getWords cur input = map (increment cur) $ fromMaybe [] $ (:) <$> word <*> (getWords <$> (loc <$> word) <*> remainder)
  where
    word = (fst <$> getWord input)
    remainder = (snd <$> getWord input)

getWord :: Text -> Maybe (IndexedWord, Text)
getWord input = if (input' == "") then Nothing else Just (IndexedWord (length r1) (pack r2'), r3)
  where
    (r1, input') = break isAlphaNum input
    (r2, r3) = span isAlphaNum input'
    r2' = ((unpack r2) =~ ("[a-zA-Z0-9_-]+" :: String) :: String)

parseEditorState :: Text -> EditorState
parseEditorState input = EditorState (to_i $ ns !! 0) (to_i $ ns !! 1) (to_i $ ns !! 2) (to_i $ ns !! 3) (to_i $ ns !! 4) (unpack $ ns !! 5)
    where
      ns :: [Text]
      ns = split (== ',') input
      to_i = read . unpack
