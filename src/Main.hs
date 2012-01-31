{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Monad (forM_)
import System.Random (randomRIO)

import qualified Data.Text as T

import Sihemo
import Sihemo.Client
import Sihemo.Types

randomCheck :: IO Bool
randomCheck = do
    random <- randomRIO (0 :: Int, 10)
    return $ random > 0

main :: IO ()
main = runSihemoWith (\_ -> return ()) $ \monitor ->
    forM_ [1 :: Int .. 5] $ \i ->
        forM_ (services i) $ \s ->
            checkEvery monitor s 10 12 randomCheck
  where
    services nr = map (Service group) ["usb", "gyrid", "ping", "ssh"]
      where
        group = "gyrid-" `T.append` (T.pack (show nr))
