{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)

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
    monitor <- newMonitor $ \_ _ -> return ()
    _       <- forkIO $ demo monitor
    serve monitor
