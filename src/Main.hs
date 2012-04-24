{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Sihemo (runSihemo)

main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [p] -> runSihemo (read p)
        _   -> do
            putStrLn $ "Usage: " ++ progName ++ " <listen port>"
            exitFailure
