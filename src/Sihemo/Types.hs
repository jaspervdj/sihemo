{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Types
    ( Service (..)
    , ServiceState (..)
    , ServiceSnapshot (..)
    , Heartbeat (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (mzero)
import Data.Ord (comparing)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as A

data Service = Service
    { serviceGroup :: Text
    , serviceName  :: Text
    } deriving (Eq)

instance Ord Service where
    compare = comparing (serviceGroup &&& serviceName)

instance ToJSON Service where
    toJSON (Service group name) = A.object
        [ "group" A..= group
        , "name"  A..= name
        ]

instance FromJSON Service where
    parseJSON (A.Object o) = Service
        <$> o A..: "group" A..!= "(no group)"
        <*> o A..: "name"  A..!= "(no name)"
    parseJSON _            = mzero

instance Show Service where
    show (Service group name) = T.unpack group ++ "/" ++ T.unpack name

data ServiceState = Up | Down | Shutdown
    deriving (Eq, Show)

instance ToJSON ServiceState where
    toJSON Up       = "up"
    toJSON Down     = "down"
    toJSON Shutdown = "shutdown"

data ServiceSnapshot = ServiceSnapshot Service ServiceState

instance ToJSON ServiceSnapshot where
    toJSON (ServiceSnapshot service state) = A.object
        [ "service" A..= service
        , "state"   A..= state
        ]

data Heartbeat = Heartbeat
    { heartbeatService :: Service
    , heartbeatAlive   :: Int
    } deriving (Show)
