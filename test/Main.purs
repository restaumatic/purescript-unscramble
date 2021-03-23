module Test.Main where

import Prelude

import Effect
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Unscramble (decodeJSON, class Decode)
import Effect.Aff (Aff, launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec
import Test.Spec.Assertions
import Foreign.Object as Object
import Data.Set as Set

type TestRecord = { a :: Int, b :: Int }

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Unscramble.Decode" do
    let
      testDecode :: forall a. Eq a => Show a => Decode a => String -> Maybe a -> Spec Unit
      testDecode json result = do
        it (json <> " -> " <> maybe "fail" show result) do
          decodeJSON json `shouldEqual` result

    describe "invalid JSON" do
      testDecode "trash" (Nothing :: Maybe String)

    describe "string" do
      testDecode "\"hello\"" (Just "hello")
      testDecode "1" (Nothing :: Maybe String)

    describe "number" do
      testDecode "1.23" (Just 1.23)
      testDecode "\"hello\"" (Nothing :: Maybe Number)

    describe "int" do
      testDecode "1" (Just 1)
      testDecode "1.23" (Nothing :: Maybe Int)
      testDecode "\"hello\"" (Nothing :: Maybe Int)

    describe "boolean" do
      testDecode "true" (Just true)
      testDecode "false" (Just false)
      testDecode "\"hello\"" (Nothing :: Maybe Boolean)

    describe "array" do
      testDecode "[]" (Just [] :: Maybe (Array Int))
      testDecode "[1,2,3]" (Just [1,2,3] :: Maybe (Array Int))
      testDecode "1" (Nothing :: Maybe (Array Int))

    describe "Set" do
      testDecode "[]" (Just mempty :: Maybe (Set.Set Int))
      testDecode "[1,2,3]" (Just (Set.fromFoldable [1,2,3]) :: Maybe (Set.Set Int))
      testDecode "1" (Nothing :: Maybe (Set.Set Int))

    describe "object" do
      testDecode "{}" (Just Object.empty :: Maybe (Object.Object Int))
      testDecode """ { "a": 1, "b": 2 } """ (Just (Object.fromFoldable [ Tuple "a" 1, Tuple "b" 2 ]))
      testDecode "1" (Nothing :: Maybe (Object.Object Int))
      testDecode "[]" (Nothing :: Maybe (Object.Object Int))

    describe "record" do
      testDecode """ {} """ (Just {})
      testDecode """ { "a": 1, "b": 2 } """ (Just { a: 1, b: 2 })
      testDecode """ { "a": 1, "b": 2, "c": 3 } """ (Just { a: 1, b: 2 })
      testDecode """ { "a": 1 } """ (Nothing :: Maybe TestRecord)
      testDecode """ { "a": 1, "b": [] } """ (Nothing :: Maybe TestRecord)

    describe "Maybe" do
      testDecode """ null """ (Just (Nothing :: Maybe Int))
      testDecode """ 1 """ (Just (Just 1 :: Maybe Int))
      testDecode """ "foo" """ (Nothing :: Maybe (Maybe Int))
      testDecode """ {} """ (Just { a: Nothing :: Maybe Int })

    describe "array of records" do
      testDecode """ [ { "a": 1, "b": 2 }, { "a":3, "b":4 } ] """ (Just [{ a: 1, b: 2 }, { a: 3, b: 4 }])
