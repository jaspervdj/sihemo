{-# LANGUAGE OverloadedStrings #-}
module Sihemo.Types
    ( Service (..)
    , ServiceState (..)
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
    , serviceId    :: Text
    , serviceName  :: Text
    } deriving (Show)

instance Eq Service where
    s1 == s2 = serviceGroup s1 == serviceGroup s2 &&
        serviceId s1 == serviceId s2

instance Ord Service where
    compare = comparing (serviceGroup &&& serviceId) 

instance ToJSON Service where
    toJSON (Service group id' name) = A.object
        [ "group" A..= group
        , "id"    A..= id'
        , "name"  A..= name
        ]

instance FromJSON Service where
    parseJSON (A.Object o) = Service
        <$> o A..: "group"
        <*> o A..: "id"
        <*> o A..: "name"
    parseJSON _            = mzero

data ServiceState = Up | Down
    deriving (Eq, Show)

instance ToJSON ServiceState where
    toJSON Up   = "up"
    toJSON Down = "down"

data Heartbeat = Heartbeat
    { heartbeatService :: Service
    , heartbeatNext    :: Int
    } deriving (Show)

instance ToJSON Heartbeat where
    toJSON (Heartbeat service next) = A.object
        [ "service" A..= service
        , "next"    A..= next
        ]

instance FromJSON Heartbeat where
    parseJSON (A.Object o) = Heartbeat
        <$> o A..: "service"
        <*> o A..: "next"
    parseJSON _            = mzero
