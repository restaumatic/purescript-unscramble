module Unscramble.Generic where

import Prelude

import Unscramble
import Foreign
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product, Sum(..), from, to)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Prim.RowList as RL

genericUnsafeDecode :: forall a rep. Generic a rep => GenericDecode rep => Options -> Foreign -> a
genericUnsafeDecode opts = to <<< genericDecoder opts

-- No options supported yet
type Options = {}

defaultOptions :: Options
defaultOptions = {}

-- This is the top-level typeclass. It dispatches to either single unpacked constructor, or to sum decoding by tag.
class GenericDecode a where
  genericDecoder :: Options -> Foreign -> a

instance genericDecodeSingleConstructor :: GenericDecodeArgs args => GenericDecode (Constructor name args) where
  genericDecoder opts = Constructor <<< genericDecodeArgs

else instance genericDecodeSumType :: GenericDecodeSum a => GenericDecode a where
  genericDecoder opts =
    let constructors = Object.fromFoldable (genericSumDecoder opts :: Array (Tuple String (Foreign -> a)))
    in \value ->
      let tag = decodeString (unsafeCoerce value).tag in
      case Object.lookup tag constructors of
        Nothing ->
          decodingError $ "Invalid sum type tag: " <> show tag
        Just decoder ->
          decoder value

class GenericDecodeSum a where
  genericSumDecoder :: Options -> Array (Tuple String (Foreign -> a))

instance genericDecodeSingleRecordArgumentConstructor ::
  ( RL.RowToList r rl
  , DecodeRecord rl
  , IsSymbol name
  ) => GenericDecodeSum (Constructor name (Argument (Record r))) where
  genericSumDecoder opts =
    [ Tuple
        (reflectSymbol (SProxy :: SProxy name))
        (Constructor <<< Argument <<< unsafeDecode)
    ]
else instance genericDecodeConstructor ::
  ( GenericDecodeArgs args
  , IsSymbol name
  ) => GenericDecodeSum (Constructor name args) where
  genericSumDecoder opts =
    [ Tuple
        (reflectSymbol (SProxy :: SProxy name))
        (Constructor <<< genericDecodeArgs <<< _.contents <<< unsafeCoerce)
    ]

class GenericDecodeArgs a where
  genericDecodeArgs :: Foreign -> a

instance genericDecodeArgsNoArguments :: GenericDecodeArgs NoArguments where
  genericDecodeArgs _ = NoArguments

instance genericDecodeArgsSingleArg :: Decode a => GenericDecodeArgs (Argument a) where
  genericDecodeArgs = Argument <<< unsafeDecode

instance genericDecodeArgsProduct :: GenericDecodeArgs (Product a b) where
  genericDecodeArgs _ = decodingError "TODO: unimplemented"

instance genericDecodeSum :: (GenericDecodeSum a, GenericDecodeSum b) => GenericDecodeSum (Sum a b) where
  genericSumDecoder opts =
    map (map (map Inl)) (genericSumDecoder opts) <>
    map (map (map Inr)) (genericSumDecoder opts)
