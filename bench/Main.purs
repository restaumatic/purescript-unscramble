module Bench.Main where

import Prelude
import Unscramble as U
import Foreign.Generic as F
import Effect
import Foreign
import Data.Maybe
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Effect.Exception (throw)
import Data.Either (Either(..), either)
import Data.List.NonEmpty as NE

type Size = String

newtype Product = Product
  { name :: String
  , description :: String
  , photo :: Maybe String
  , url :: String
  , enabled :: Boolean
  , containsAllergens :: Boolean
  , sizes :: Array Size
  , price :: Array Number
  , additions :: Array Addition
  }

derive newtype instance U.Decode Product
derive newtype instance F.Decode Product
derive instance Eq Product

newtype Addition = Addition
  { name :: String
  , description :: String
  , photo :: Maybe String
  , price :: Array Number
  , enabled :: Boolean
  , minQuantity :: Int
  , maxQuantity :: Maybe Int
  }

derive newtype instance U.Decode Addition
derive newtype instance F.Decode Addition
derive instance Eq Addition

foreign import generateData :: Effect Foreign

foreign import measure :: forall a. String -> (Unit -> a) -> Effect Unit

test :: Foreign -> String -> (Foreign -> Either String (Array Product)) -> Effect (Either String (Array Product))
test input name decoder = do
  measure name (\_ -> decoder input)

  let result = decoder input
  case result of
    Left err ->
      throw $ "decoder " <> name <> " failed: " <> err
    Right _ ->
      pure result

main :: Effect Unit
main = do
  input <- generateData
  result1 <- test input "Unscramble Record" U.decodeEither
  result2 <- test input "Foreign.Generic Record" (either (Left <<< renderForeignError <<< NE.head) Right <<< runExcept <<< F.decode)

  unless (result1 == result2) do
    throw "results differ"

  pure unit
