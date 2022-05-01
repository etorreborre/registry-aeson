{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.DecoderSpec where

import Data.Aeson
import Data.Registry
import Data.Registry.Aeson.Decoder
import Data.Time
import qualified Data.Vector as V
import Protolude
import Test.Data.Registry.Aeson.DataTypes
import Test.Tasty.Hedgehogx

test_decode = test "decode" $ do
  decodeValue (make @(Decoder Delivery) decoders) (array [Number 0]) === Right delivery0
  decodeValue (make @(Decoder Delivery) decoders) (array [Number 1, "me@here.com"]) === Right delivery1
  decodeValue (make @(Decoder Delivery) decoders) (array [Number 2, array [Number 123, "me@here.com"], "2022-04-18T00:00:12.000Z"]) === Right delivery2

-- * HELPERS

decoders :: Registry _ _
decoders = end
  -- $(makeDecoder ''Delivery)
    -- <: $(makeDecoder ''Person)
    -- <: $(makeDecoder ''Email)
    -- <: $(makeDecoder ''Identifier)
    <: fun datetimeDecoder
    <: jsonDecoder @Text
    <: jsonDecoder @Int


datetimeDecoder :: Decoder DateTime
datetimeDecoder = Decoder $ \case
  String s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure (DateTime t)
      Nothing -> Left ("cannot read a DateTime: " <> s)
  other -> Left $ "not a valid DateTime: " <> show other

-- | Shortcut function to create arrays
array :: [Value] -> Value
array = Array . V.fromList
