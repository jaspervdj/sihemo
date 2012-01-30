{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Web
    ( serve
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)

import qualified Data.Aeson as A
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import Sihemo.Monitor

data WebEnv = WebEnv
    { webMonitor :: Monitor
    }

type Web = ReaderT WebEnv Snap.Snap

services :: Web ()
services = do
    monitor <- webMonitor <$> ask
    states' <- liftIO $ getStates monitor
    Snap.modifyResponse $ Snap.setContentType "application/json"
    Snap.writeLBS $ A.encode $ map json states'
  where
    json (serv, st) = A.object ["service" A..= serv, "state" A..= st]

site :: Web ()
site = Snap.route
    -- TODO: use actual data directory
    [ ("",              Snap.ifTop $ Snap.serveFile "data/index.html")
    , ("services.json", services)
    , ("subscribe",     return ())
    ] <|> Snap.serveDirectory "data"

serve :: Monitor -> IO ()
serve monitor =
    Snap.httpServe Snap.defaultConfig $ runReaderT site env
  where
    env = WebEnv monitor
