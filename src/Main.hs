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
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy.Char8 as LC (unpack)
import Control.Applicative
import Control.Exception (fromException, catch, SomeException)
import Control.Monad (forever)
import Network
import System.IO

import Types

main :: IO ()
main = withSocketsDo $ do
    -- hSetBuffering stdout NoBuffering
    (flip catch) ex_catch $ do
      sock <- listenOn (PortNumber 8001)
      WS.runServer "0.0.0.0" 9160 $ application sock
      sClose sock
    putStrLn "Connection closed."
      where
        ex_catch :: SomeException -> IO ()
        ex_catch e = putStrLn $ "Exception caught." ++ show e
        application :: Socket -> WS.Request -> WS.WebSockets WS.Hybi00 ()
        application sock rq = do
            WS.acceptRequest rq
            sink <- WS.getSink
            flip WS.catchWsError (catchDisconnect sink) $
              forever $ do
                liftIO $ do
                  h <- getHandle sock
                  cnt <- (pack <$> hGetLine h)
                  output <- mainHandler cnt
                  responseWebsocket sink output
                  responseHttp h
              where
                catchDisconnect sink e = case fromException e of
                    Just WS.ConnectionClosed -> liftIO $ WS.sendSink sink $ WS.textData ("disconnected" :: Text)
                    _ -> return ()
                getHandle :: Socket -> IO Handle
                getHandle sock' = do
                    (h,_,_) <- accept sock'
                    hSetBuffering h LineBuffering
                    return h
                responseHttp :: Handle -> IO ()
                responseHttp h = mapM_ (hPutStrLn h) ["HTTP/1.0 200 OK", "Content-type: text/html", "Content-Length: 0", ""]
                responseWebsocket :: WS.Sink WS.Hybi00 -> Text -> IO ()
                responseWebsocket sink output' = WS.sendSink sink $ WS.textData output'

------------------------------------------------

mainHandler :: Text -> IO Text
mainHandler cnt' = do
    let editor_state = parseEditorState cnt'
    let fname = editingFile editor_state
    cnt <- pack <$> (liftIO $ readFile fname)
    let nodes' = getNodes cnt
    return $ convertToJSON $ Whole nodes' [] editor_state

convertToJSON :: Whole -> Text
convertToJSON whole = pack $ LC.unpack $ encode $ toJSON whole

getNodes :: Text -> [Node]
getNodes cnt = map (updateGrepCount cnt) $ L.concat $ map (\(l, line) -> map (getNode l) $ getWords 1 line) $ zip [1..] $ lines cnt

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
getWord input = if (input' == "" || r2' == "") then Nothing else Just (IndexedWord (length r1) (pack r2'), r3)
  where
    (r1, input') = break isAlphaNum input
    (r2, r3) = span isAlphaNum input'
    r2' = ((unpack r2) =~ ("[a-zA-Z0-9_-]*" :: String) :: String)

parseEditorState :: Text -> EditorState
parseEditorState input = EditorState (to_i $ ns !! 0) (to_i $ ns !! 1) (to_i $ ns !! 2) (to_i $ ns !! 3) (to_i $ ns !! 4) (unpack $ ns !! 5)
    where
      ns :: [Text]
      ns = split (== ',') input
      to_i = read . unpack
