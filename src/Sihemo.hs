module Sihemo
    ( runSihemo
    , runSihemoWith
    ) where

import qualified Data.Aeson as A
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Util.PubSub as WS

import Sihemo.Monitor
import Sihemo.Types
import Sihemo.Web

-- | Start the web server. This function blocks forever.
runSihemo :: Int    -- ^ Web port
          -> IO ()  -- ^ Blocks forever
runSihemo = runSihemoWith noHook noHook
  where
    noHook _ = return ()

-- | A variant of 'runSihemo' which allows you to pass some custom hooks.
--
-- The snapshot hook is called every time the status of some service changes.
--
-- The setup hook is only called once at initialization. It allows you to
-- register some custom checks with the monitor.
runSihemoWith :: (ServiceSnapshot -> IO ())  -- ^ Snapshot hook
              -> (Monitor -> IO ())          -- ^ Setup hook
              -> Int                         -- ^ Web port
              -> IO ()                       -- ^ Blocks forever
runSihemoWith hook setupMonitor port = do
    pubSub  <- WS.newPubSub
    monitor <- newMonitor $ hook' pubSub
    setupMonitor monitor
    serve port monitor pubSub
  where
    hook' pubSub snapshot = do
        WS.publish pubSub $ WS.textData $ A.encode snapshot
        hook snapshot
