{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module : KD8ZRC.Mapview.Encoding.JSON
-- Copyright : (C) 2016 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : ghc
--
-- Provides an encoding for JSON encapsulation of mapview types.
----------------------------------------------------------------------------
module KD8ZRC.Mapview.Encoding.JSON
       ( MapviewJson (..)
       , PacketType (..)
       , writeChanJsonPkt) where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import KD8ZRC.Mapview.Types

data PacketType
  = Telemetry
  | Playback
  | ChaseGps
  | Internal
  | CustomPacket T.Text
  deriving (Eq, Ord, Show)

data MapviewJson t =
  MapviewJson { mapviewJsonPacketType :: PacketType
              , mapviewJsonData :: t
              }

instance ToJSON PacketType where
  toJSON Telemetry = "telemetry"
  toJSON Playback = "playback"
  toJSON ChaseGps = "chase_gps"
  toJSON Internal = "internal"
  toJSON (CustomPacket p) = toJSON p

instance FromJSON PacketType where
  parseJSON (String "telemetry") = return Telemetry
  parseJSON (String "playback") = return Playback
  parseJSON (String "chase_gps") = return ChaseGps
  parseJSON (String "internal") = return Internal
  parseJSON (String s) = return (CustomPacket s)
  parseJSON _ = mzero

instance ToJSON t => ToJSON (MapviewJson t) where
    toJSON (MapviewJson pt d) =
        object ["packet_type" .= pt, "data" .= d]

instance FromJSON t => FromJSON (MapviewJson t) where
    parseJSON (Object v) = MapviewJson <$> v .: "packet_type"
                                       <*> v .: "data"
    parseJSON _ = mzero

-- | Write a JSON-encoded telemetry packet to a channel in the correct format.
writeChanJsonPkt
  :: ToJSON t
  => Chan.Chan BS.ByteString
  -> ParsedPacketCallback t
writeChanJsonPkt ch =
  ParseSuccessCallback (
    \pkt -> liftIO $
            Chan.writeChan ch (
              BSL.toStrict . encode . MapviewJson Telemetry $ pkt))
