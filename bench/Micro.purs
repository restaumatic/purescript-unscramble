module Bench.Micro where

import Prelude
import Unscramble as U
import Unscramble.Enum as U
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

-- A value which should have negligible decoding overhead.
newtype Value = Value Foreign

derive newtype instance encodeValue :: F.Encode Value
derive newtype instance decodeValueU :: U.Decode Value
derive newtype instance decodeValueF :: F.Decode Value
derive newtype instance decodeValueSJ :: SJ.ReadForeign Value
instance eqValue :: Eq Value where
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

derive newtype instance encodeR3 :: F.Encode R3
derive newtype instance decodeR3U :: U.Decode R3
derive newtype instance decodeR3F :: F.Decode R3
derive newtype instance decodeR3SJ :: SJ.ReadForeign R3
derive instance eqR3 :: Eq R3

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

derive newtype instance encodeR10 :: F.Encode R10
derive newtype instance decodeR10U :: U.Decode R10
derive newtype instance decodeR10F :: F.Decode R10
derive newtype instance decodeR10SJ :: SJ.ReadForeign R10
derive instance eqR10 :: Eq R10

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

derive newtype instance encodeR30 :: F.Encode R30
derive newtype instance decodeR30U :: U.Decode R30
derive newtype instance decodeR30F :: F.Decode R30
derive newtype instance decodeR30SJ :: SJ.ReadForeign R30
derive instance eqR30 :: Eq R30

data Enum3 = E3_1 | E3_2 | E3_3

derive instance genericEnum3 :: Generic Enum3 _
instance encodeEnum3 :: F.Encode Enum3 where encode = F.genericEncodeEnum F.defaultGenericEnumOptions
instance decodeEnum3U :: U.Decode Enum3 where unsafeDecode = U.genericUnsafeDecodeEnum U.defaultEnumOptions
instance decodeEnum3F :: F.Decode Enum3 where decode = F.genericDecodeEnum F.defaultGenericEnumOptions

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

derive instance genericEnum10 :: Generic Enum10 _
instance encodeEnum10 :: F.Encode Enum10 where encode = F.genericEncodeEnum F.defaultGenericEnumOptions
instance decodeEnum10U :: U.Decode Enum10 where unsafeDecode = U.genericUnsafeDecodeEnum U.defaultEnumOptions
instance decodeEnum10F :: F.Decode Enum10 where decode = F.genericDecodeEnum F.defaultGenericEnumOptions

foreign import measure :: forall a. String -> (Unit -> a) -> Effect Unit

type Decoder a = Tuple String (Foreign -> Either String a)

unscramble :: forall a. U.Decode a => Decoder a
unscramble = Tuple "Unscramble" U.decodeEither

foreignGeneric :: forall a. F.Decode a => Decoder a
foreignGeneric = Tuple "Foreign.Generic" (either (Left <<< renderForeignError <<< NE.head) Right <<< runExcept <<< F.decode)

simpleJson :: forall a. SJ.ReadForeign a => Decoder a
simpleJson = Tuple "Simple.JSON" (either (Left <<< renderForeignError <<< NE.head) Right <<< SJ.read)

test :: forall a. F.Encode a => String -> (Int -> a) -> Array (Decoder a) -> Effect Unit
test name generator decoders = do
  let input = F.encode (generator 0)

  for_ decoders \(Tuple decoderName decoder) ->
    case decoder input of
      Left err ->
        throw $ "decoder " <> decoderName <> " failed: " <> err
      Right _ ->
        pure unit

  -- TODO: check if results are the same

  for_ decoders \(Tuple decoderName decoder) ->
    measure (name <> " " <> decoderName) (\_ -> decoder input)

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

allDecoders = [ unscramble, foreignGeneric, simpleJson ]
genericDecoders = [ unscramble, foreignGeneric ]

main :: Effect Unit
main = do
  test "Enum3" genEnum3 genericDecoders
  test "Array(100) Enum3" (genArray 100 genEnum3) genericDecoders

  test "Enum10" genEnum10 genericDecoders
  test "Array(100) Enum10" (genArray 100 genEnum10) genericDecoders

  test "Array(100)" (genArray 100 genValue) allDecoders
  test "Array(1000)" (genArray 1000 genValue) allDecoders
  test "Array(10000)" (genArray 10000 genValue) allDecoders

  test "R3" genR3 allDecoders
  test "R10" genR10 allDecoders
  test "Array(100) R10" (genArray 100 genR10) allDecoders
  test "R30" genR30 allDecoders
  test "Array(100) R30" (genArray 100 genR30) allDecoders
