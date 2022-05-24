module Unscramble where

import Prelude

import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Foreign.Object (Object)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList as RL
import Foreign
import Data.FoldableWithIndex
import Data.Foldable
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Data.Set as Set
import Data.Map as Map

decode :: forall a. Decode a => Foreign -> Maybe a
decode value = catchDecodingError (\_ -> unsafeDecode value)

type DecodingError = String

type Result = Either DecodingError

decodeEither :: forall a. Decode a => Foreign -> Result a
decodeEither value = catchDecodingErrorEither (\_ -> unsafeDecode value)

decodeJSON :: forall a. Decode a => String -> Maybe a
decodeJSON value = catchDecodingError (\_ -> unsafeDecode (unsafeParseJSON value))

decodeJSONEither :: forall a. Decode a => String -> Result a
decodeJSONEither value = catchDecodingErrorEither (\_ -> unsafeDecode (unsafeParseJSON value))

foreign import decodingError :: forall a. String -> a

class Decode a where
  unsafeDecode :: Foreign -> a

instance decode_Foreign :: Decode Foreign where
  unsafeDecode x = x

instance decode_Unit :: Decode Unit where
  unsafeDecode _ = unit

instance decode_String :: Decode String where
  unsafeDecode = decodeString

instance decode_Number :: Decode Number where
  unsafeDecode = decodeNumber

instance decode_Int :: Decode Int where
  unsafeDecode = decodeInt

instance decode_Boolean :: Decode Boolean where
  unsafeDecode = decodeBoolean

instance decode_Array :: Decode a => Decode (Array a) where
  unsafeDecode = decodeArray unsafeDecode

instance decode_NonEmptyArray :: Decode a => Decode (NE.NonEmptyArray a) where
  unsafeDecode f =
    case NE.fromArray (decodeArray unsafeDecode f) of
      Just nonEmpty ->
        nonEmpty
      Nothing -> 
        decodingError "NonEmptyArray should not be empty"

instance decode_Tuple :: (Decode a, Decode b) => Decode (Tuple a b) where
  unsafeDecode v =
    case expectArray v of
      [a, b] -> Tuple (unsafeDecode a) (unsafeDecode b)
      _ -> decodingError "Invalid array length in Tuple representation"

instance decode_Object :: Decode a => Decode (Object a) where
  unsafeDecode = decodeObject unsafeDecode

instance decode_Record :: (RL.RowToList r rl, DecodeRecord rl) => Decode (Record r) where
  unsafeDecode = decodeRecord (recordInfo (Proxy :: Proxy rl))

instance decode_Maybe :: Decode a => Decode (Maybe a) where
  unsafeDecode value =
      if isNull value || isUndefined value then
        Nothing
      else
        Just (unsafeDecode value)

instance decode_Set :: (Decode a, Ord a) => Decode (Set.Set a) where
  unsafeDecode = Set.fromFoldable <<< decodeArray unsafeDecode

-- Map decoding

class FromJSONKey k where
  fromJSONKey :: FromJSONKeyFunction k

instance fromJSONKeyString :: FromJSONKey String where
  fromJSONKey = FromJSONKeyString identity

defaultFromJSONKeyValue :: forall k. Decode k => FromJSONKeyFunction k
defaultFromJSONKeyValue = FromJSONKeyValue unsafeDecode

data FromJSONKeyFunction k = FromJSONKeyString (String -> k) | FromJSONKeyValue (Foreign -> k)

instance decode_Map :: (FromJSONKey k, Ord k, Decode v) => Decode (Map.Map k v) where
  unsafeDecode v =
    case fromJSONKey :: FromJSONKeyFunction k of
      FromJSONKeyString decodeKey ->
        foldlWithIndex (\k acc v -> Map.insert (decodeKey k) (unsafeDecode v) acc) Map.empty (expectObject v)
      FromJSONKeyValue decodeKey ->
        foldl (\acc (Tuple k v) -> Map.insert (decodeKey k) (unsafeDecode v) acc) Map.empty (unsafeDecode v :: Array (Tuple Foreign Foreign))

-- Internal and very unsafe - TODO: move to Internal

foreign import data RecordInfo :: Type
foreign import recordInfoNil :: RecordInfo
foreign import recordInfoCons :: forall a. String -> (Foreign -> a) -> RecordInfo -> RecordInfo
foreign import decodeRecord :: forall r. RecordInfo -> Foreign -> Record r

class DecodeRecord rl where
  recordInfo :: Proxy rl -> RecordInfo

instance decodeRecordNil :: DecodeRecord RL.Nil where
  recordInfo _ = recordInfoNil

instance decodeRecordCons :: (IsSymbol label, Decode a, DecodeRecord rest) => DecodeRecord (RL.Cons label a rest) where
  recordInfo _ = recordInfoCons
    (reflectSymbol (Proxy :: Proxy label))
    (unsafeDecode :: Foreign -> a)
    (recordInfo (Proxy :: Proxy rest))

foreign import isString :: Foreign -> Boolean
foreign import decodeString :: Foreign -> String
foreign import isNumber :: Foreign -> Boolean
foreign import decodeNumber :: Foreign -> Number
foreign import decodeInt :: Foreign -> Int
foreign import decodeBoolean :: Foreign -> Boolean
foreign import decodeArray :: forall a. (Foreign -> a) -> Foreign -> Array a
foreign import decodeObject :: forall a. (Foreign -> a) -> Foreign -> Object a
foreign import expectObject :: Foreign -> Object Foreign
foreign import expectArray :: Foreign -> Array Foreign
foreign import unsafeParseJSON :: String -> Foreign

catchDecodingError :: forall a. (Unit -> a) -> Maybe a
catchDecodingError = catchDecodingErrorImpl (\_ -> Nothing) Just

catchDecodingErrorEither :: forall a. (Unit -> a) -> Either String a
catchDecodingErrorEither = catchDecodingErrorImpl Left Right

foreign import catchDecodingErrorImpl :: forall a r. (String -> r) -> (a -> r) -> (Unit -> a) -> r
