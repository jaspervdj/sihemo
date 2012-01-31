{-# LANGUAGE BangPatterns #-}
module Sihemo.Monitor
    ( Monitor
    , newMonitor
    , getSnapshots
    , heartbeat
    , shutdown
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Monad (unless)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M

import Sihemo.Types

type Tick = Int

type Monitor_ = Map Service (ServiceState, Tick)

getServiceState :: Service -> Monitor_ -> (ServiceState, Tick)
getServiceState service = fromMaybe (Shutdown, 0) . M.lookup service

data Monitor = Monitor
    { monitorServices :: MVar Monitor_
    , monitorHook     :: ServiceSnapshot -> IO ()
    }

-- | Create a new monitor
newMonitor :: (ServiceSnapshot -> IO ())  -- ^ Up/down hook
           -> IO Monitor                          -- ^ Resulting monitor
newMonitor hook = Monitor <$> MV.newMVar M.empty <*> pure hook

-- | Get the current state of the services
getSnapshots :: Monitor -> IO [ServiceSnapshot]
getSnapshots monitor =
    map mkSnapshot . M.toList <$> MV.readMVar (monitorServices monitor)
  where
    mkSnapshot (x, (y, _)) = ServiceSnapshot x y

-- | Run hooks, if necessary
runHook :: Monitor       -- ^ Monitor
        -> Service       -- ^ Possibly updated service
        -> ServiceState  -- ^ Old state
        -> ServiceState  -- ^ New state
        -> IO ()         -- ^ Calls hook and returns
runHook monitor service state state' = unless (state == state') $
    monitorHook monitor $ ServiceSnapshot service state'

-- | Send a heartbeat to the monitor
heartbeat :: Monitor -> Heartbeat -> IO ()
heartbeat monitor hb = MV.modifyMVar_ (monitorServices monitor) $ \servs -> do
    let (state, tick) = getServiceState service servs
        !tick'        = tick + 1
    runHook monitor service state Up
    _ <- forkIO $ watchdog monitor hb tick'
    return $ M.insert service (Up, tick') servs
  where
    service = heartbeatService hb

-- | Code which runs in the background to check a service
watchdog :: Monitor -> Heartbeat -> Int -> IO ()
watchdog monitor hb tick = do
    threadDelay (heartbeatAlive hb * 1000 * 1000)
    MV.modifyMVar_ (monitorServices monitor) $ \services -> do
        let (state, tick') = getServiceState service services
        if state /= Up || tick' /= tick
            then return services
            else do
                runHook monitor service state Down
                return $ M.insert service (Down, tick) services
  where
    service = heartbeatService hb

-- | Safely shut down a service
shutdown :: Monitor -> Service -> IO ()
shutdown monitor serv = MV.modifyMVar_ (monitorServices monitor) $ \servs -> do
    let (state, _) = getServiceState serv servs
    runHook monitor serv state Shutdown
    return $ M.delete serv servs
