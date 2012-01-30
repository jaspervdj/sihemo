{-# LANGUAGE BangPatterns #-}
module Sihemo.Monitor
    ( Monitor
    , newMonitor
    , heartbeat
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M

import qualified Data.Text as T

import Sihemo.Types

type Tick = Int

type Monitor_ = Map Service (ServiceState, Tick)

getState :: Service -> Monitor_ -> (ServiceState, Tick)
getState service = fromMaybe (Up, 0) . M.lookup service

newtype Monitor = Monitor {unMonitor :: MVar Monitor_}

newMonitor :: IO Monitor
newMonitor = Monitor <$> MV.newMVar M.empty

hooks :: Service -> ServiceState -> ServiceState -> IO ()
hooks service Up   Down = putStrLn $ show (serviceName service) ++ " DOWN!"
hooks service Down Up   = putStrLn $ show (serviceName service) ++ " UP!"
hooks _       _    _    = return ()

heartbeat :: Monitor -> Heartbeat -> IO ()
heartbeat monitor hb = MV.modifyMVar_ (unMonitor monitor) $ \services -> do
    let (state, tick) = getState service services
        !tick'        = tick + 1
    hooks service state Up
    _ <- forkIO $ watchdog monitor hb tick'
    return $ M.insert service (Up, tick') services
  where
    service = heartbeatService hb

watchdog :: Monitor -> Heartbeat -> Int -> IO ()
watchdog (Monitor mvar) hb tick = do
    threadDelay (heartbeatNext hb * 1000 * 1000)
    MV.modifyMVar_ mvar $ \services -> do
        let (state, tick') = getState service services

        if tick' /= tick
            then return services
            else do
                hooks service state Down
                return $ M.insert service (Down, tick) services
  where
    service = heartbeatService hb
