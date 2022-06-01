module Bench.Micro where

import Prelude
import Unscramble as U
import Unscramble.Generic as U
import Unscramble.Enum as U
import Foreign as F
import Foreign.Generic as F
import Foreign.Generic.EnumEncoding as F
import Effect
import Foreign
import Data.Maybe
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Effect.Exception (throw)
import Data.Either (Either(..), either)
import Data.List.NonEmpty as NE
import Data.Tuple
import Data.Foldable
import Data.Array as Array
import Simple.JSON as SJ
import Data.Generic.Rep
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut as A
import Data.Argonaut.Decode.Generic as A
import Data.Argonaut.Types.Generic as A
import Effect.Class.Console as Console
import Data.String as String

-- A value which should have negligible decoding overhead.
newtype Value = Value Foreign

derive newtype instance F.Encode Value
derive newtype instance U.Decode Value
derive newtype instance F.Decode Value
derive newtype instance SJ.ReadForeign Value
instance A.DecodeJson Value where
  decodeJson = pure <<< Value <<< F.unsafeToForeign
instance Eq Value where
  eq (Value x) (Value y) = F.encodeJSON x == F.encodeJSON y

newtype R3 = R3
  { f1 :: Value
  , f2 :: Value
  , f3 :: Value
  }

def :: Value
def = Value (F.encode 1)

genValue :: Int -> Value
genValue _ = def

derive instance Generic R3 _
derive newtype instance F.Encode R3
derive newtype instance U.Decode R3
derive newtype instance F.Decode R3
derive newtype instance SJ.ReadForeign R3
derive newtype instance A.DecodeJson R3
derive instance Eq R3

newtype R10 = R10
  { f1 :: Value
  , f2 :: Value
  , f3 :: Value
  , f4 :: Value
  , f5 :: Value
  , f6 :: Value
  , f7 :: Value
  , f8 :: Value
  , f9 :: Value
  , f10 :: Value
  }

derive instance Generic R10 _
derive newtype instance F.Encode R10
derive newtype instance U.Decode R10
derive newtype instance F.Decode R10
derive newtype instance SJ.ReadForeign R10
derive newtype instance A.DecodeJson R10
derive instance Eq R10

newtype R30 = R30
  { f1 :: Value
  , f2 :: Value
  , f3 :: Value
  , f4 :: Value
  , f5 :: Value
  , f6 :: Value
  , f7 :: Value
  , f8 :: Value
  , f9 :: Value
  , f10 :: Value
  , f11 :: Value
  , f12 :: Value
  , f13 :: Value
  , f14 :: Value
  , f15 :: Value
  , f16 :: Value
  , f17 :: Value
  , f18 :: Value
  , f19 :: Value
  , f20 :: Value
  , f21 :: Value
  , f22 :: Value
  , f23 :: Value
  , f24 :: Value
  , f25 :: Value
  , f26 :: Value
  , f27 :: Value
  , f28 :: Value
  , f29 :: Value
  , f30 :: Value
  }

derive instance Generic R30 _
derive newtype instance F.Encode R30
derive newtype instance U.Decode R30
derive newtype instance F.Decode R30
derive newtype instance SJ.ReadForeign R30
derive newtype instance A.DecodeJson R30
derive instance Eq R30

data Enum3 = E3_1 | E3_2 | E3_3

derive instance Generic Enum3 _
instance F.Encode Enum3 where encode = F.genericEncodeEnum F.defaultGenericEnumOptions
instance U.Decode Enum3 where unsafeDecode = U.genericUnsafeDecodeEnum U.defaultEnumOptions
instance F.Decode Enum3 where decode = F.genericDecodeEnum F.defaultGenericEnumOptions
instance A.DecodeJson Enum3 where decodeJson = A.decodeLiteralSum

data Enum10
  = E10_1
  | E10_2
  | E10_3
  | E10_4
  | E10_5
  | E10_6
  | E10_7
  | E10_8
  | E10_9
  | E10_10

derive instance Generic Enum10 _
instance F.Encode Enum10 where encode = F.genericEncodeEnum F.defaultGenericEnumOptions
instance U.Decode Enum10 where unsafeDecode = U.genericUnsafeDecodeEnum U.defaultEnumOptions
instance F.Decode Enum10 where decode = F.genericDecodeEnum F.defaultGenericEnumOptions
instance A.DecodeJson Enum10 where decodeJson = A.decodeLiteralSum

data Enum30
  = E30_1
  | E30_2
  | E30_3
  | E30_4
  | E30_5
  | E30_6
  | E30_7
  | E30_8
  | E30_9
  | E30_10
  | E30_11
  | E30_12
  | E30_13
  | E30_14
  | E30_15
  | E30_16
  | E30_17
  | E30_18
  | E30_19
  | E30_20
  | E30_21
  | E30_22
  | E30_23
  | E30_24
  | E30_25
  | E30_26
  | E30_27
  | E30_28
  | E30_29
  | E30_30

derive instance Generic Enum30 _
instance F.Encode Enum30 where encode = F.genericEncodeEnum F.defaultGenericEnumOptions
instance U.Decode Enum30 where unsafeDecode = U.genericUnsafeDecodeEnum U.defaultEnumOptions
instance F.Decode Enum30 where decode = F.genericDecodeEnum F.defaultGenericEnumOptions
instance A.DecodeJson Enum30 where decodeJson = A.decodeLiteralSum

type SumArg = { value :: Value }

sumArg :: SumArg
sumArg = { value: def }

data Sum3
  = S3_1 SumArg
  | S3_2 SumArg
  | S3_3 SumArg

derive instance Generic Sum3 _

data Sum10
  = S10_1 SumArg
  | S10_2 SumArg
  | S10_3 SumArg
  | S10_4 SumArg
  | S10_5 SumArg
  | S10_6 SumArg
  | S10_7 SumArg
  | S10_8 SumArg
  | S10_9 SumArg
  | S10_10 SumArg

derive instance Generic Sum10 _

data Sum30
  = S30_1 SumArg
  | S30_2 SumArg
  | S30_3 SumArg
  | S30_4 SumArg
  | S30_5 SumArg
  | S30_6 SumArg
  | S30_7 SumArg
  | S30_8 SumArg
  | S30_9 SumArg
  | S30_10 SumArg
  | S30_11 SumArg
  | S30_12 SumArg
  | S30_13 SumArg
  | S30_14 SumArg
  | S30_15 SumArg
  | S30_16 SumArg
  | S30_17 SumArg
  | S30_18 SumArg
  | S30_19 SumArg
  | S30_20 SumArg
  | S30_21 SumArg
  | S30_22 SumArg
  | S30_23 SumArg
  | S30_24 SumArg
  | S30_25 SumArg
  | S30_26 SumArg
  | S30_27 SumArg
  | S30_28 SumArg
  | S30_29 SumArg
  | S30_30 SumArg

derive instance Generic Sum30 _

foreign import measure :: forall a. String -> (Unit -> a) -> Effect Unit

foreignToJson :: Foreign -> A.Json
foreignToJson = unsafeCoerce

type Decoder a = Tuple String (Foreign -> Either String a)

unscramble :: forall a. U.Decode a => Decoder a
unscramble = Tuple "Unscramble" U.decodeEither

foreignGeneric :: forall a. F.Decode a => Decoder a
foreignGeneric = Tuple "Foreign.Generic" (either (Left <<< renderForeignError <<< NE.head) Right <<< runExcept <<< F.decode)

simpleJson :: forall a. SJ.ReadForeign a => Decoder a
simpleJson = Tuple "Simple.JSON" (either (Left <<< renderForeignError <<< NE.head) Right <<< SJ.read)

argonaut :: forall a. A.DecodeJson a => Decoder a
argonaut = Tuple "Argonaut" (either (Left <<< A.printJsonDecodeError) Right <<< A.decodeJson <<< foreignToJson)

foreign import getFilters :: Effect (Array String)

test :: forall a. F.Encode a => String -> (Int -> a) -> Array (Decoder a) -> Effect Unit
test name generator decoders' = do
  let input = F.encode (generator 0)

  filters <- getFilters
  let decoders = Array.filter (\(Tuple decoderName _) -> matchesFilters filters (name <> " " <> decoderName)) decoders'

  for_ decoders \(Tuple decoderName decoder) ->
    case decoder input of
      Left err ->
        throw $ "decoder " <> decoderName <> " failed: " <> err
      Right _ ->
        pure unit

  -- TODO: check if results are the same

  for_ decoders \(Tuple decoderName decoder) ->
    measure (name <> " " <> decoderName) (\_ -> decoder input)

  unless (Array.null decoders) do
    Console.log "---"

  where
    matchesFilters [] _ = true
    matchesFilters filters name = any (\filter -> String.contains (String.Pattern filter) name) filters

genR3 :: Int -> R3
genR3 _ = R3 { f1: def, f2: def, f3: def }

genR10 :: Int -> R10
genR10 _ = R10
  { f1: def
  , f2: def
  , f3: def
  , f4: def
  , f5: def
  , f6: def
  , f7: def
  , f8: def
  , f9: def
  , f10: def
  }

genR30 :: Int -> R30
genR30 _ = R30
  { f1: def
  , f2: def
  , f3: def
  , f4: def
  , f5: def
  , f6: def
  , f7: def
  , f8: def
  , f9: def
  , f10: def
  , f11: def
  , f12: def
  , f13: def
  , f14: def
  , f15: def
  , f16: def
  , f17: def
  , f18: def
  , f19: def
  , f20: def
  , f21: def
  , f22: def
  , f23: def
  , f24: def
  , f25: def
  , f26: def
  , f27: def
  , f28: def
  , f29: def
  , f30: def
  }

genArray :: forall a. Int -> (Int -> a) -> Int -> Array a
genArray n gen _ = gen <$> Array.range 0 n

genEnum3 :: Int -> Enum3
genEnum3 n =
  case n `mod` 3 of
    1 -> E3_1
    2 -> E3_2
    _ -> E3_3

genEnum10 :: Int -> Enum10
genEnum10 n =
  case n `mod` 10 of
    1 -> E10_1
    2 -> E10_2
    3 -> E10_3
    4 -> E10_4
    5 -> E10_5
    6 -> E10_6
    7 -> E10_7
    8 -> E10_8
    9 -> E10_9
    _ -> E10_10

genEnum30 :: Int -> Enum30
genEnum30 n =
  case n `mod` 30 of
    1 -> E30_1
    2 -> E30_2
    3 -> E30_3
    4 -> E30_4
    5 -> E30_5
    6 -> E30_6
    7 -> E30_7
    8 -> E30_8
    9 -> E30_9
    10 -> E30_10
    11 -> E30_11
    12 -> E30_12
    13 -> E30_13
    14 -> E30_14
    15 -> E30_15
    16 -> E30_16
    17 -> E30_17
    18 -> E30_18
    19 -> E30_19
    20 -> E30_20
    21 -> E30_21
    22 -> E30_22
    23 -> E30_23
    24 -> E30_24
    25 -> E30_25
    26 -> E30_26
    27 -> E30_27
    28 -> E30_28
    29 -> E30_29
    _ -> E30_30

newtype GenericWrapper a = GenericWrapper a

unwrap :: forall a. GenericWrapper a -> a
unwrap (GenericWrapper x) = x

foreignGenericOpts :: F.Options
foreignGenericOpts = F.defaultOptions { unwrapSingleConstructors = true }

instance (Generic a rep, F.GenericEncode rep) => F.Encode (GenericWrapper a) where
  encode = F.genericEncode foreignGenericOpts <<< unwrap

instance (Generic a rep, U.GenericDecode rep) => U.Decode (GenericWrapper a) where
  unsafeDecode = GenericWrapper <<< U.genericUnsafeDecode U.defaultOptions

instance (Generic a rep, F.GenericDecode rep) => F.Decode (GenericWrapper a) where
  decode = map GenericWrapper <<< F.genericDecode foreignGenericOpts

instance (Generic a rep, A.DecodeRep rep) => A.DecodeJson (GenericWrapper a) where
  decodeJson = map GenericWrapper <<< A.genericDecodeJsonWith defaultArgonautEncoding

defaultArgonautEncoding :: A.Encoding
defaultArgonautEncoding =
  { tagKey: "tag"
  , valuesKey: "contents"
  , unwrapSingleArguments: true
  }

genSum3 :: Int -> Sum3
genSum3 n =
  case n `mod` 3 of
    1 -> S3_1 sumArg
    2 -> S3_2 sumArg
    _ -> S3_3 sumArg

genSum10 :: Int -> Sum10
genSum10 n =
  case n `mod` 10 of
    1 -> S10_1 sumArg
    2 -> S10_2 sumArg
    3 -> S10_3 sumArg
    4 -> S10_4 sumArg
    5 -> S10_5 sumArg
    6 -> S10_6 sumArg
    7 -> S10_7 sumArg
    8 -> S10_8 sumArg
    9 -> S10_9 sumArg
    _ -> S10_10 sumArg

genSum30 :: Int -> Sum30
genSum30 n =
  case n `mod` 30 of
    1 -> S30_1 sumArg
    2 -> S30_2 sumArg
    3 -> S30_3 sumArg
    4 -> S30_4 sumArg
    5 -> S30_5 sumArg
    6 -> S30_6 sumArg
    7 -> S30_7 sumArg
    8 -> S30_8 sumArg
    9 -> S30_9 sumArg
    10 -> S30_10 sumArg
    11 -> S30_11 sumArg
    12 -> S30_12 sumArg
    13 -> S30_13 sumArg
    14 -> S30_14 sumArg
    15 -> S30_15 sumArg
    16 -> S30_16 sumArg
    17 -> S30_17 sumArg
    18 -> S30_18 sumArg
    19 -> S30_19 sumArg
    20 -> S30_20 sumArg
    21 -> S30_21 sumArg
    22 -> S30_22 sumArg
    23 -> S30_23 sumArg
    24 -> S30_24 sumArg
    25 -> S30_25 sumArg
    26 -> S30_26 sumArg
    27 -> S30_27 sumArg
    28 -> S30_28 sumArg
    29 -> S30_29 sumArg
    _  -> S30_30 sumArg


genGeneric g n = GenericWrapper (g n)

allDecoders = [ unscramble, argonaut, foreignGeneric, simpleJson ]
genericDecoders = [ unscramble, argonaut, foreignGeneric ]
genericSingleConstructorDecoders = [ unscramble, foreignGeneric ]

main :: Effect Unit
main = do
  filters <- getFilters
  Console.log $ "Filters: " <> show filters

  test "Generic Sum3"  (genGeneric genSum3 ) genericDecoders
  test "Generic Sum10" (genGeneric genSum10) genericDecoders
  test "Generic Sum30" (genGeneric genSum30) genericDecoders

  test "Array(100) Generic Sum10" (genArray 100 (genGeneric genSum10)) genericDecoders

  test "R3" genR3 allDecoders
  test "R10" genR10 allDecoders
  test "R30" genR30 allDecoders

  test "Generic R3"  (genGeneric genR3 ) genericSingleConstructorDecoders
  test "Generic R10" (genGeneric genR10) genericSingleConstructorDecoders
  test "Generic R30" (genGeneric genR30) genericSingleConstructorDecoders

  test "Int" (\i -> i) allDecoders
  test "Array(100) Int" (genArray 100 \i -> i) allDecoders
  test "Array(1000) Int" (genArray 1000 \i -> i) allDecoders

  test "Enum3" genEnum3 genericDecoders
  test "Enum10" genEnum10 genericDecoders
  test "Enum30" genEnum30 genericDecoders

  test "Array(100) Enum3" (genArray 100 genEnum3) genericDecoders
  test "Array(100) Enum10" (genArray 100 genEnum10) genericDecoders
  test "Array(100) Enum30" (genArray 100 genEnum30) genericDecoders

  test "Array(100)" (genArray 100 genValue) allDecoders
  test "Array(1000)" (genArray 1000 genValue) allDecoders
  test "Array(10000)" (genArray 10000 genValue) allDecoders

  test "Array(100) R3" (genArray 100 genR3) allDecoders
  test "Array(100) R10" (genArray 100 genR10) allDecoders
  test "Array(100) R30" (genArray 100 genR30) allDecoders
