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
       ( MapviewJson ()
       , mkMapviewJson
       , writeChanJsonPkt) where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import KD8ZRC.Mapview.Types

data MapviewJson t =
  MapviewJson { mapviewJsonPacketType :: T.Text
              , mapviewJsonData :: t
              }

mkMapviewJson :: ToJSON t => T.Text -> t -> MapviewJson t
mkMapviewJson = MapviewJson

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
              BSL.toStrict . encode . mkMapviewJson "telemetry" $ pkt))
