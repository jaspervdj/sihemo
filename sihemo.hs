{-# LANGUAGE OverloadedStrings #-}
module Main where

import Sihemo
import Sihemo.Monitor
import Sihemo.Sendmail
import Sihemo.Types

main :: IO ()
main = runSihemoWith snapshotHook setupHook 8002

snapshotHook :: ServiceSnapshot -> IO ()
snapshotHook (ServiceSnapshot _       Up)       = return ()
snapshotHook (ServiceSnapshot _       Shutdown) = return ()
snapshotHook (ServiceSnapshot service Down)     = do
    sendmail "xxx@example.com" text [text]
  where
    text = "sihemo: Down: " ++ show service

setupHook :: Monitor -> IO ()
setupHook _ = return ()
