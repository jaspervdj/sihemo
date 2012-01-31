-- | A module to allow the user to write checks which run in the same process as
-- the monitor, and talk to it directly, instead of through a REST interface.
module Sihemo.Client
    ( checkEvery
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)

import Sihemo.Monitor
import Sihemo.Types

-- | This function installs a watchdog which checks every @n@ seconds whether or
-- not a service is up. If the check succeeds, the monitor will consider the
-- service to be "alive" for another @m@ seconds. @m@ should be larger than @n@,
-- @m = 2 * n@ is usually a good value.
checkEvery :: Monitor  -- ^ Monitor handle
           -> Service  -- ^ Service descriptor
           -> Int      -- ^ @n@
           -> Int      -- ^ @m@
           -> IO Bool  -- ^ Check to run
           -> IO ()    -- ^ Immediately returns
checkEvery monitor service seconds alive check = do
    heartbeat monitor $ Heartbeat service 1
    -- TODO: check for exceptions in 'check'
    _ <- forkIO $ forever $ do
        result <- check
        when result $ heartbeat monitor $ Heartbeat service alive
        threadDelay $ seconds * 1000 * 1000
    return ()
