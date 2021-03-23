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

derive newtype instance decodeProductU :: U.Decode Product
derive newtype instance decodeProductF :: F.Decode Product
derive instance eqProduct :: Eq Product

newtype Addition = Addition
  { name :: String
  , description :: String
  , photo :: String
  , price :: Array Number
  , enabled :: Boolean
  , minQuantity :: Int
  , maxQuantity :: Maybe Int
  }

derive newtype instance decodeAdditionU :: U.Decode Addition
derive newtype instance decodeAdditionF :: F.Decode Addition
derive instance eqAddition :: Eq Addition

foreign import generateData :: Effect Foreign

foreign import measure :: forall a. String -> (Unit -> a) -> Effect Unit

test :: String -> (Foreign -> Maybe (Array Product)) -> Effect (Maybe (Array Product))
test name decoder = do
  input <- generateData
  measure name (\_ -> decoder input)

  let result = decoder input
  when (isNothing result) do
    throw $ "decoder " <> name <> " failed"
  pure result

main :: Effect Unit
main = do
  result1 <- test "Unscramble Record" U.decode
  result2 <- test "Foreign.Generic Record" (hush <<< runExcept <<< F.decode)

  unless (result1 == result2) do
    throw "results differ"

  pure unit
