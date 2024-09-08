module Unscramble.Enum where

import Prelude

import Unscramble (decodeString, decodingError)
import Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Foreign.Object as Object

genericUnsafeDecodeEnum :: forall a rep. Generic a rep => EnumConstructors rep a => EnumOptions -> Foreign -> a
genericUnsafeDecodeEnum opts =
  let constructors = Object.fromFoldable (enumConstructors to opts :: Array (Tuple String a))
  in \value ->
    let tag = decodeString value in
    case Object.lookup tag constructors of
      Nothing ->
        decodingError $ "Invalid enum tag value: " <> show tag
      Just x ->
        x

type EnumOptions = { constructorTagTransform :: String -> String }

defaultEnumOptions :: EnumOptions
defaultEnumOptions = { constructorTagTransform: identity }

class EnumConstructors a r where
  enumConstructors :: (a -> r) -> EnumOptions -> Array (Tuple String r)

instance IsSymbol name => EnumConstructors (Constructor name NoArguments) r where
  enumConstructors construct opts =
    [ Tuple
        (opts.constructorTagTransform $ reflectSymbol (Proxy :: Proxy name))
        (construct (Constructor NoArguments))
    ]

instance (EnumConstructors a r, EnumConstructors b r) => EnumConstructors (Sum a b) r where
  enumConstructors construct opts =
    enumConstructors (construct <<< Inl) opts <>
    enumConstructors (construct <<< Inr) opts
