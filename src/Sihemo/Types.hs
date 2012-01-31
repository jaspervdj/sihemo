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

import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as A

data Service = Service
    { serviceGroup :: Text
    , serviceName  :: Text
    } deriving (Eq, Show)

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

data ServiceState = Up | Down
    deriving (Eq, Show)

instance ToJSON ServiceState where
    toJSON Up   = "up"
    toJSON Down = "down"

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
