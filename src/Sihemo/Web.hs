{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Web
    ( serve
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Network.WebSockets.Util.PubSub as WS
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import Sihemo.Monitor (Monitor)
import Sihemo.Types
import qualified Sihemo.Monitor as Monitor

data WebEnv = WebEnv
    { webMonitor :: Monitor
    , webPubSub  :: WS.PubSub WS.Hybi00
    }

type Web = ReaderT WebEnv Snap.Snap

index :: Web ()
index = Snap.serveFile "data/index.html"

services :: Web ()
services = do
    monitor <- webMonitor <$> ask
    states' <- liftIO $ Monitor.getSnapshots monitor
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode $ states'

heartbeat :: Web ()
heartbeat = do
    monitor <- webMonitor <$> ask
    malive  <- fmap (read . BC.unpack) <$> Snap.getParam "alive"
    mgroup  <- fmap T.decodeUtf8 <$> Snap.getParam "group"
    mname   <- fmap T.decodeUtf8 <$> Snap.getParam "name"
    case (mgroup, mname) of
        (Just group, Just name) -> liftIO $ Monitor.heartbeat monitor $
            Heartbeat (Service group name) (fromMaybe 30 malive)
        _                       -> fail "Invalid request"

subscribe :: Web ()
subscribe = do
    pubSub <- webPubSub <$> ask
    Snap.liftSnap $ WS.runWebSocketsSnap $ app pubSub
  where
    app pubSub req = do
        WS.acceptRequest req
        WS.subscribe pubSub

site :: Web ()
site = Snap.route
    -- TODO: use actual data directory
    [ ("/",                                Snap.ifTop $ index)
    , ("/services.json",                   services)
    , ("/services/:group/:name/heartbeat", Snap.method Snap.POST heartbeat)
    , ("/subscribe",                       subscribe)
    ] <|> Snap.serveDirectory "data"

serve :: Monitor -> WS.PubSub WS.Hybi00 -> IO ()
serve monitor pubSub =
    Snap.httpServe Snap.defaultConfig $ runReaderT site env
  where
    env = WebEnv monitor pubSub
