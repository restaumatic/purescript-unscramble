module Test.Main where

import Prelude

import Effect
import Data.Maybe (Maybe(..), maybe)
import Unscramble (decodeJSON, class Decode)
import Effect.Aff (Aff, launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec
import Test.Spec.Assertions

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
