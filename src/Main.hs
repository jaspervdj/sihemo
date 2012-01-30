module Main
    ( main
    ) where

import Sihemo.Monitor
import Sihemo.Web

main :: IO ()
main = do
    monitor <- newMonitor $ \_ _ -> return ()
    serve monitor
