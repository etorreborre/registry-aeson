{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Aeson.DecoderSpec where

import Data.Aeson hiding (decode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL (fromStrict)
import Data.Registry
import Data.Registry.Aeson.Decoder
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Protolude
import Test.Data.Registry.Aeson.DataTypes
import Test.Tasty.Hedgehogx hiding (maybe, text)

test_decode = test "decode" $ do
  checkDecoding "123" (Identifier 123)
  -- decodeByteString (make @(Decoder Delivery) decoders) "NoDelivery" === Right delivery0
  -- decodeByteString (make @(Decoder Delivery) decoders) (array [Number 1, "me@here.com"]) === Right delivery1
  -- decodeByteString (make @(Decoder Delivery) decoders) (array [Number 2, array [Number 123, "me@here.com"], "2022-04-18T00:00:12.000Z"]) === Right delivery2

-- * HELPERS

checkDecoding :: forall a. (FromJSON a, Typeable a, Eq a, Show a) => Text -> a -> PropertyT IO ()
checkDecoding = withFrozenCallStack . checkDecodingWith defaultOptions

checkDecodingWith :: forall a. (FromJSON a, Typeable a, Eq a, Show a) => Options -> Text -> a -> PropertyT IO ()
checkDecodingWith options text a = withFrozenCallStack $ do
  let input = BL.fromStrict . T.encodeUtf8 $ T.replace "'" "\"" text
  let decoder = make @(Decoder a) (val options <: decoders)
  let asValue = decodeByteString decoder input
  let asGeneric = A.decode input

  annotate "the decoded Value must be the same as the generic one"
  asValue === maybe (Left "wrong") Right asGeneric

  annotate "the decoded Value must be the expected value"
  asValue === Right a

decoders :: Registry _ _
decoders = end
  -- $(makeDecoder ''Delivery)
    -- <: $(makeDecoder ''Person)
    -- <: $(makeDecoder ''Email)
    <: $(makeDecoder ''Identifier)
    <: fun datetimeDecoder
    <: jsonDecoder @Text
    <: jsonDecoder @Int
    <: val defaultOptions


datetimeDecoder :: Decoder DateTime
datetimeDecoder = Decoder $ \case
  String s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure (DateTime t)
      Nothing -> Left ("cannot read a DateTime: " <> s)
  other -> Left $ "not a valid DateTime: " <> show other
