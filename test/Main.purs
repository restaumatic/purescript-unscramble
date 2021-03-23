module Test.Main where

import Prelude

import Effect
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec
import Test.Spec.Assertions

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Unscramble.Decode" do
    it "works" do
      pure unit
