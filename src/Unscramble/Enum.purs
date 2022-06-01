module Unscramble.Enum where

import Prelude

import Unscramble
import Foreign
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Data.Generic.Rep (class Generic, Argument, Constructor(..), NoArguments(..), Product, Sum(..), from, to)
import Foreign.Object as Object

genericUnsafeDecodeEnum :: forall a rep. Generic a rep => EnumConstructors rep => EnumOptions -> Foreign -> a
genericUnsafeDecodeEnum opts =
  let constructors = Object.fromFoldable (enumConstructors opts :: Array (Tuple String rep))
  in \value ->
    let tag = decodeString value in
    case Object.lookup tag constructors of
      Nothing ->
        decodingError $ "Invalid enum tag value: " <> show tag
      Just x ->
        to x

type EnumOptions = { constructorTagTransform :: String -> String }

defaultEnumOptions :: EnumOptions
defaultEnumOptions = { constructorTagTransform: identity }

class EnumConstructors a where
  enumConstructors :: EnumOptions -> Array (Tuple String a)

instance IsSymbol name => EnumConstructors (Constructor name NoArguments) where
  enumConstructors opts =
    [ Tuple
        (opts.constructorTagTransform $ reflectSymbol (Proxy :: Proxy name))
        (Constructor NoArguments)
    ]

instance (EnumConstructors a, EnumConstructors b) => EnumConstructors (Sum a b) where
  enumConstructors opts =
    map (map Inl) (enumConstructors opts) <>
    map (map Inr) (enumConstructors opts)
