{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Web
    ( serve
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Network.WebSockets.Util.PubSub as WS
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import Paths_sihemo
import Sihemo.Monitor (Monitor)
import Sihemo.Types
import qualified Sihemo.Monitor as Monitor

data WebEnv = WebEnv
    { webMonitor :: Monitor
    , webPubSub  :: WS.PubSub WS.Hybi10
    , webDataDir :: FilePath
    }

type Web = ReaderT WebEnv Snap.Snap

writeAeson :: (A.ToJSON a, Snap.MonadSnap m) => a -> m ()
writeAeson x = do
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode x

index :: Web ()
index = do
    dataDir <- webDataDir <$> ask
    Snap.serveFile $ dataDir </> "index.html"

services :: Web ()
services = do
    monitor <- webMonitor <$> ask
    states' <- liftIO $ Monitor.getStates monitor
    writeAeson states'

withService :: (Service -> Web ()) -> Web ()
withService f = do
    mgroup <- fmap T.decodeUtf8 <$> Snap.getParam "group"
    mname  <- fmap T.decodeUtf8 <$> Snap.getParam "name"
    case (mgroup, mname) of
        (Just group, Just name) -> f $ Service group name
        _                       -> fail "Invalid request"

service :: Web ()
service = withService $ \serv -> do
    monitor <- webMonitor <$> ask
    state   <- liftIO $ Monitor.getState monitor serv
    writeAeson $ ServiceSnapshot serv state

-- | Delegates to 'down' or 'heartbeat'
postService :: Web ()
postService = do
    state <- Snap.getParam "state"
    case state of
        Just "down" -> down
        _           -> heartbeat

heartbeat :: Web ()
heartbeat = withService $ \serv -> do
    monitor <- webMonitor <$> ask
    malive  <- fmap (read . BC.unpack) <$> Snap.getParam "alive"
    liftIO $ Monitor.heartbeat monitor $ Heartbeat serv (fromMaybe 30 malive)

down :: Web ()
down = withService $ \serv -> do
    monitor <- webMonitor <$> ask
    liftIO $ Monitor.down monitor serv

shutdown :: Web ()
shutdown = withService $ \serv -> do
    monitor <- webMonitor <$> ask
    liftIO $ Monitor.shutdown monitor serv

subscribe :: Web ()
subscribe = do
    pubSub <- webPubSub <$> ask
    Snap.liftSnap $ WS.runWebSocketsSnap $ app pubSub
  where
    app pubSub req = do
        WS.acceptRequest req
        WS.spawnPingThread 10
        WS.subscribe pubSub

site :: Web ()
site = do
    dataDir <- webDataDir <$> ask
    Snap.route
        [ ("/",                      Snap.ifTop $ index)
        , ("/services",              Snap.method Snap.GET services)
        , ("/services/:group/:name", Snap.method Snap.GET service)
        , ("/services/:group/:name", Snap.method Snap.POST postService)
        , ("/services/:group/:name", Snap.method Snap.DELETE shutdown)
        , ("/subscribe",             subscribe)
        ] <|> Snap.serveDirectory dataDir

serve :: Monitor -> WS.PubSub WS.Hybi10 -> IO ()
serve monitor pubSub = do
    dataDir <- getDataDir
    Snap.httpServe Snap.defaultConfig $ runReaderT site $ env dataDir
  where
    env = WebEnv monitor pubSub
