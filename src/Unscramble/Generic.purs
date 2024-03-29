module Unscramble.Generic where

import Prelude

import Unscramble
import Foreign
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Prim.RowList as RL
import Partial.Unsafe (unsafePartial)

genericUnsafeDecode :: forall a rep. Generic a rep => GenericDecode rep => Options -> Foreign -> a
genericUnsafeDecode opts = to <<< genericDecoder opts

-- | Decode a sum type generically, without unwrapping single constructors (i.e. always requiring tagged representation).
-- 
-- NB: the reason this is implemented as a separate function instead of an option is to
-- minimize the dependencies of the genericDecodeSingleConstructor instance - the variant is
-- chosen at the type level.
genericUnsafeDecodeTagged :: forall a rep. Generic a rep => GenericDecodeSum rep => Options -> Foreign -> a
genericUnsafeDecodeTagged opts = to <<< genericDecodeTaggedSumType opts

-- No options supported yet
type Options = {}

defaultOptions :: Options
defaultOptions = {}

-- This is the top-level typeclass. It dispatches to either single unpacked constructor, or to sum decoding by tag.
class GenericDecode a where
  genericDecoder :: Options -> Foreign -> a

instance GenericDecodeArgs args => GenericDecode (Constructor name args) where
  genericDecoder opts = Constructor <<< genericDecodeArgs

else instance GenericDecodeSum a => GenericDecode a where
  genericDecoder = genericDecodeTaggedSumType
  
genericDecodeTaggedSumType :: forall a. GenericDecodeSum a => Options -> Foreign -> a
genericDecodeTaggedSumType opts =
  let constructors = Object.fromFoldable (genericSumDecoder opts :: Array (Tuple String (Foreign -> a)))
  in \value ->
    let tag = decodeString (unsafeCoerce (expectObject value)).tag in
    case Object.lookup tag constructors of
      Nothing ->
        decodingError $ "Invalid sum type tag: " <> show tag
      Just decoder ->
        decoder value

class GenericDecodeSum a where
  genericSumDecoder :: Options -> Array (Tuple String (Foreign -> a))

class DecodeSingleConstructorArgument a where
  unsafeDecodeSingleConstructorArgument :: Foreign -> a

instance
  ( RL.RowToList r rl
  , DecodeRecord rl
  ) => DecodeSingleConstructorArgument (Record r) where
  unsafeDecodeSingleConstructorArgument = unsafeDecode

else instance
  ( Decode a
  ) => DecodeSingleConstructorArgument a where
  unsafeDecodeSingleConstructorArgument = unsafeDecode <<< unwrapContents

instance
  ( IsSymbol name
  , DecodeSingleConstructorArgument a
  ) => GenericDecodeSum (Constructor name (Argument a)) where
  genericSumDecoder _opts =
    [ Tuple
        (reflectSymbol (Proxy :: Proxy name))
        (Constructor <<< Argument <<< unsafeDecodeSingleConstructorArgument)
    ]
else instance
  ( GenericDecodeArgs args
  , IsSymbol name
  ) => GenericDecodeSum (Constructor name args) where
  genericSumDecoder _opts =
    [ Tuple
        (reflectSymbol (Proxy :: Proxy name))
        (Constructor <<< genericDecodeArgs <<< unwrapContents)
    ]

unwrapContents :: Foreign -> Foreign
unwrapContents = _.contents <<< unsafeCoerce

class GenericDecodeArgs a where
  genericDecodeArgs :: Foreign -> a

instance GenericDecodeArgs NoArguments where
  genericDecodeArgs _ = NoArguments

else instance Decode a => GenericDecodeArgs (Argument a) where
  genericDecodeArgs = Argument <<< unsafeDecode

else instance GenericDecodeManyArgs args => GenericDecodeArgs args where
  genericDecodeArgs =
    let Tuple numArgs decode = argsDecoder :: Tuple Int (Int -> Array Foreign -> args)
    in \value ->
      let array = expectArray value
      in if Array.length array == numArgs then
           decode 0 array
         else
           decodingError "Invalid number of constructor arguments"

class GenericDecodeManyArgs a where
  argsDecoder :: Tuple Int (Int -> Array Foreign -> a)

instance (GenericDecodeManyArgs a, GenericDecodeManyArgs b) => GenericDecodeManyArgs (Product a b) where
  argsDecoder =
    let Tuple args1 decode1 = argsDecoder :: Tuple Int (Int -> Array Foreign -> a)
        Tuple args2 decode2 = argsDecoder :: Tuple Int (Int -> Array Foreign -> b)
    in Tuple (args1 + args2) (\offset array -> Product (decode1 offset array) (decode2 (offset + args1) array))

instance Decode a => GenericDecodeManyArgs (Argument a) where
  argsDecoder = Tuple 1 (\offset array -> Argument (unsafeDecode (unsafePartial (Array.unsafeIndex array offset))))

instance (GenericDecodeSum a, GenericDecodeSum b) => GenericDecodeSum (Sum a b) where
  genericSumDecoder opts =
    map (map (map Inl)) (genericSumDecoder opts) <>
    map (map (map Inr)) (genericSumDecoder opts)
