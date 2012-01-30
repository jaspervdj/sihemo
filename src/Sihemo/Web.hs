{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Web
    ( serve
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)

import qualified Data.Aeson as A
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import qualified Network.WebSockets.Util.PubSub as WS
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import Sihemo.Monitor (Monitor)
import qualified Sihemo.Monitor as Monitor

data WebEnv = WebEnv
    { webMonitor :: Monitor
    , webPubSub  :: WS.PubSub WS.Hybi00
    }

type Web = ReaderT WebEnv Snap.Snap

services :: Web ()
services = do
    monitor <- webMonitor <$> ask
    states' <- liftIO $ Monitor.getSnapshots monitor
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode $ states'

heartbeat :: Web ()
heartbeat = do
    monitor <- webMonitor <$> ask
    body    <- B.concat . BL.toChunks <$> Snap.readRequestBody 4096
    case A.parseOnly A.json body of
        Left _  -> failWith "Could not parse JSON body"
        Right v -> case A.fromJSON v of
            A.Error _    -> failWith "Invalid JSON data"
            A.Success hb -> liftIO $ Monitor.heartbeat monitor hb
  where
    failWith msg = do
        Snap.modifyResponse $ Snap.setResponseStatus 500 "Internal server error"
        Snap.writeBS msg

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
    [ ("",              Snap.ifTop $ Snap.serveFile "data/index.html")
    , ("services.json", services)
    , ("heartbeat",     Snap.method Snap.POST heartbeat)
    , ("subscribe",     subscribe)
    ] <|> Snap.serveDirectory "data"

serve :: Monitor -> WS.PubSub WS.Hybi00 -> IO ()
serve monitor pubSub =
    Snap.httpServe Snap.defaultConfig $ runReaderT site env
  where
    env = WebEnv monitor pubSub
