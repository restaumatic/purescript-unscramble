module Unscramble where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign

decode :: forall a. Decode a => Foreign -> Maybe a
decode value = catchDecodingError (unsafeDecode value)

decodeJSON :: forall a. Decode a => String -> Maybe a
decodeJSON value = catchDecodingError (unsafeDecode (unsafeParseJSON value))

foreign import decodingError :: forall a. Partial => String -> a

class Decode a where
  unsafeDecode :: Partial => Foreign -> a

instance decode_Foreign :: Decode Foreign where
  unsafeDecode x = x

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

-- Internal - TODO: move to Internal

foreign import decodeString :: Foreign -> String
foreign import decodeNumber :: Foreign -> Number
foreign import decodeInt :: Foreign -> Int
foreign import decodeBoolean :: Foreign -> Boolean
foreign import decodeArray :: forall a. (Foreign -> a) -> Foreign -> Array a
foreign import unsafeParseJSON :: String -> Foreign

catchDecodingError :: forall a. (Partial => a) -> Maybe a
catchDecodingError = catchDecodingErrorImpl (\_ -> Nothing) Just

foreign import catchDecodingErrorImpl :: forall a r. (String -> r) -> (a -> r) -> (Partial => a) -> r
