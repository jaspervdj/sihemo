{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)

import qualified Data.Aeson as A
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import Sihemo.Types
import Sihemo.Monitor
import Sihemo.Web

demo :: Monitor -> IO ()
demo monitor = forever $ do
    heartbeat monitor $ Heartbeat service 5
    random <- randomRIO (2, 10)
    threadDelay $ random * 1000 * 1000
  where
    service = Service "demo" "demo-1" "Demo 1"

main :: IO ()
main = do
    pubSub  <- WS.newPubSub
    monitor <- newMonitor $ WS.publish pubSub . WS.textData . A.encode
    _       <- forkIO $ demo monitor
    serve monitor pubSub
