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
  checkDecoding "{'_email':'me@here.com'}" email1
  checkDecoding "{'email':{'_email':'me@here.com'},'identifier':123}" person1
  checkDecoding "{'_datetime':'2022-04-18T00:00:12Z'}" datetime1
  checkDecoding "{'tag':'NoDelivery'}" delivery0
  checkDecoding "{'tag':'ByEmail','contents':{'_email':'me@here.com'}}" delivery1
  checkDecoding "{'tag':'InPerson','contents':[{'email':{'_email':'me@here.com'},'identifier':123},{'_datetime':'2022-04-18T00:00:12Z'}]}" delivery2

test_all_nullary_to_string_tag = test "allNullaryToStringTag" $ do
  checkDecodingWith allNullaryOptions "'AllNullary1'" AllNullary1
  checkDecodingWith allNullaryOptions "'AllNullary2'" AllNullary2

test_field_modifier = test "fieldLabelModifier" $ do
  checkDecodingWith fieldLabelModifierOptions "{'tag':'FieldLabelModifier1','__field1':123}" (FieldLabelModifier1 123)

test_constructor_modifier = test "constructorTagModifier" $ do
  checkDecodingWith constructorTagModifierOptions "{'tag':'__ConstructorTagModifier1','ctField1':123}" (ConstructorTagModifier1 123)

test_omit_nothing_fields = test "omitNothingFields" $ do
  checkDecodingWith omitNothingFieldsOptions "{'tag':'OmitNothingFields1','onField2':123}" (OmitNothingFields1 Nothing 123)

test_unwrap_unary_records = test "unwrapUnaryRecords" $ do
  checkDecodingWith unwrapUnaryRecordsOptions "123" (UnwrapUnaryRecords1 123)

test_tag_single_constructors = test "TagSingleConstructors" $ do
  checkDecodingWith tagSingleConstructorsOptions "{'tag':'TagSingleConstructors1', 'tsField1':123}" (TagSingleConstructors1 123)

test_untagged_values_sum_encoding = test "UntaggedValueSumEncoding" $ do
  checkDecodingWith untaggedValueOptions "{'uvField1':123}" (UntaggedValueSumEncoding1 123)

test_object_with_single_field_sum_encoding = test "ObjectWithSingleFieldSumEncoding" $ do
  checkDecodingWith objectWithSingleFieldSumEncodingOptions "{'ObjectWithSingleFieldSumEncoding1':{'owsfField1':123}}" (ObjectWithSingleFieldSumEncoding1 123)

test_two_elem_array_sum_encoding = test "TwoElemArray" $ do
  checkDecodingWith twoElemArraySumEncodingOptions "['TwoElemArraySumEncoding1',{'teaField1':123}]" (TwoElemArraySumEncoding1 123)

-- * HELPERS

checkDecoding :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Text -> a -> PropertyT IO ()
checkDecoding = withFrozenCallStack . checkDecodingWith defaultOptions

checkDecodingWith :: forall a. (FromJSON a, ToJSON a, Typeable a, Eq a, Show a) => Options -> Text -> a -> PropertyT IO ()
checkDecodingWith options text a = withFrozenCallStack $ do
  let input = BL.fromStrict . T.encodeUtf8 $ T.replace "'" "\"" text
  let decoder = make @(Decoder a) (val options <: decoders)
  let asValue = decodeByteString decoder input
  let asGeneric = A.decode input

  annotate "the decoded Value must be the same as the generic one"
  annotateShow (encode a)
  asValue === maybe (Left "wrong") Right asGeneric

  annotate "the decoded Value must be the expected value"
  asValue === Right a

decoders :: Registry _ _
decoders =
  $(makeDecoder ''Delivery)
    <: $(makeDecoder ''Person)
    <: $(makeDecoder ''Email)
    <: $(makeDecoder ''Identifier)
    <: $(makeDecoder ''DateTime)
    <: $(makeDecoder ''AllNullary)
    <: $(makeDecoder ''FieldLabelModifier)
    <: $(makeDecoder ''ConstructorTagModifier)
    <: $(makeDecoder ''OmitNothingFields)
    <: $(makeDecoder ''UnwrapUnaryRecords)
    <: $(makeDecoder ''TagSingleConstructors)
    <: $(makeDecoder ''UntaggedValueSumEncoding)
    <: $(makeDecoder ''ObjectWithSingleFieldSumEncoding)
    <: $(makeDecoder ''TwoElemArraySumEncoding)
    <: fun utcTimeDecoder
    <: jsonDecoder @Text
    <: decodeMaybeOf @Int
    <: jsonDecoder @Int
    <: val defaultOptions

utcTimeDecoder :: Decoder UTCTime
utcTimeDecoder = Decoder $ \case
  String s ->
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ toS s of
      Just t -> pure t
      Nothing -> Left ("cannot read a UTCTime: " <> s)
  other -> Left $ "not a valid UTCTime: " <> show other
