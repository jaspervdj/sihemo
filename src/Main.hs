{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import Sihemo.Types
import Sihemo.Monitor
import Sihemo.Web

demo :: Monitor -> Service -> IO ()
demo monitor service = forever $ do
    heartbeat monitor $ Heartbeat service 5
    random <- randomRIO (2, 6)
    threadDelay $ random * 1000 * 1000

main :: IO ()
main = do
    pubSub  <- WS.newPubSub
    monitor <- newMonitor $ WS.publish pubSub . WS.textData . A.encode
    forM_ [1 :: Int .. 5] $ \i -> do
        forM_ (services i) $ \s -> do
            _ <- forkIO $ demo monitor s
            return ()
    serve monitor pubSub
  where
    services nr = map (Service group) ["usb", "gyrid", "ping"]
      where
        group = "gyrid-" `T.append` (T.pack (show nr))
