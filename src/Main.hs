{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forever)
import Control.Concurrent (MVar,  newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS

type ServerState = Text

main :: IO ()
main = do
    state <- newMVar ("" :: Text)
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application state rq = do
    WS.acceptRequest rq
    ver  <- WS.getVersion          -- バージョン取得
    sink <- WS.getSink
    val <- liftIO $ readMVar state -- 状態取得
    liftIO $ modifyMVar_ state $ \old_text -> do
                            WS.sendSink sink $ WS.textData ("OK" :: Text)
                            return "NEW STATE"
    talk sink state

talk :: WS.Protocol p => WS.Sink WS.Hybi00 -> MVar ServerState -> WS.WebSockets p ()
talk sink state = flip WS.catchWsError catchDisconnect $
  forever $ do
    -- データ受信時に実行される部分
    msg <- WS.receiveData
    liftIO $ do
      s1 <- readMVar state
      WS.sendSink sink $ WS.textData ("RECEIVE:" `mappend` msg :: Text)
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \old_text -> do
            WS.sendSink sink $ WS.textData ("disconnected" :: Text)
            return old_text
        _ -> return ()
