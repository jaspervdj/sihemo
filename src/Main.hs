{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Monad (forM_)
import System.Random (randomRIO)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import Sihemo.Client
import Sihemo.Monitor
import Sihemo.Types
import Sihemo.Web

randomCheck :: IO Bool
randomCheck = do
    random <- randomRIO (0 :: Int, 10)
    return $ random > 0

main :: IO ()
main = do
    pubSub  <- WS.newPubSub
    monitor <- newMonitor $ WS.publish pubSub . WS.textData . A.encode

    forM_ [1 :: Int .. 5] $ \i ->
        forM_ (services i) $ \s ->
            checkEvery monitor s 10 12 randomCheck

    serve monitor pubSub
  where
    services nr = map (Service group) ["usb", "gyrid", "ping", "ssh"]
      where
        group = "gyrid-" `T.append` (T.pack (show nr))
