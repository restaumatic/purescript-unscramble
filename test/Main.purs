module Test.Main where

import Prelude

import Effect
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Either (either, Either(..), hush)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Unscramble (decodeJSON, decodeJSONEither, Result, class Decode, class FromJSONKey, defaultFromJSONKeyValue)
import Unscramble.Generic
import Unscramble.Enum (genericUnsafeDecodeEnum, defaultEnumOptions)
import Effect.Aff (Aff, launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec
import Test.Spec.Assertions
import Foreign.Object as Object
import Data.Set as Set
import Data.Map as Map
import Data.Generic.Rep
import Data.Show.Generic

type TestRecord = { a :: Int, b :: Int }

newtype ValueKey = ValueKey String

derive newtype instance Eq ValueKey
derive newtype instance Show ValueKey
derive newtype instance Ord ValueKey
derive newtype instance Decode ValueKey

instance FromJSONKey ValueKey where
  fromJSONKey = defaultFromJSONKeyValue

data Enum = A | B | C

derive instance Generic Enum _
derive instance Eq Enum
instance Show Enum where
  show = genericShow
instance Decode Enum where
  unsafeDecode = genericUnsafeDecodeEnum defaultEnumOptions

newtype SingleConstructor a = SingleConstructor a

derive instance Generic (SingleConstructor a) _
derive instance Eq a => Eq (SingleConstructor a)
instance Show a => Show (SingleConstructor a) where
  show = genericShow
instance Decode a => Decode (SingleConstructor a) where
  unsafeDecode = genericUnsafeDecode defaultOptions

data Sum = NoArgs | SingleArg Int | SingleRecordArg TestRecord | ManyArgs TestRecord Int | ThreeArgs Int Int Int

derive instance Generic Sum _
derive instance Eq Sum
instance Show Sum where
  show = genericShow
instance Decode Sum where
  unsafeDecode = genericUnsafeDecode defaultOptions

data PolymorphicSingleConstructorArgument a = C1 | C2 a

derive instance Generic (PolymorphicSingleConstructorArgument a) _
derive instance Eq a => Eq (PolymorphicSingleConstructorArgument a)
instance Show a => Show (PolymorphicSingleConstructorArgument a) where
  show = genericShow
instance DecodeSingleConstructorArgument a => Decode (PolymorphicSingleConstructorArgument a) where
  unsafeDecode = genericUnsafeDecode defaultOptions

-- | `ForceTagged a` is decoded using `genericUnsafeDecodeTagged`
newtype ForceTagged a = ForceTagged a

derive instance Generic (ForceTagged a) _
derive newtype instance Eq a => Eq (ForceTagged a)
instance Show a => Show (ForceTagged a) where
  show = genericShow
instance (Generic a rep, GenericDecodeSum rep) => Decode (ForceTagged a) where
  unsafeDecode = ForceTagged <<< genericUnsafeDecodeTagged defaultOptions

data SingleConManyArgs = SingleConManyArgs String Int

derive instance Generic SingleConManyArgs _
derive instance Eq SingleConManyArgs
instance Show SingleConManyArgs where
  show = genericShow
instance Decode SingleConManyArgs where
  unsafeDecode = genericUnsafeDecode defaultOptions

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Unscramble.Decode" do
    let
      testDecode :: forall a. Eq a => Show a => Decode a => String -> Maybe a -> Spec Unit
      testDecode json expected = do
        it (json <> " -> " <> maybe "fail" show expected) do
          let result = decodeJSONEither json

          -- If failure is encountered when expecting success, show the error message for debugging
          case expected, result of
            Just expected', Left err ->
              fail $ "Expected " <> show expected' <> ", got " <> show err
            _, _ -> pure unit

          hush result `shouldEqual` expected

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

    describe "NonEmptyArray" do
      testDecode "[1]" (Just (NE.singleton 1) :: Maybe (NonEmptyArray Int))
      testDecode "[1,2,3]" (Just (NE.singleton 1 <> NE.singleton 2 <> NE.singleton 3) :: Maybe (NonEmptyArray Int))
      testDecode "[]" (Nothing :: Maybe (NonEmptyArray Int))
      testDecode "1" (Nothing :: Maybe (NonEmptyArray Int))

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

    describe "Tuple" do
      testDecode """ [1,"foo"] """ (Just (Tuple 1 "foo"))
      testDecode """ "foo" """ (Nothing :: Maybe (Tuple Int String))

    describe "array of records" do
      testDecode """ [ { "a": 1, "b": 2 }, { "a":3, "b":4 } ] """ (Just [{ a: 1, b: 2 }, { a: 3, b: 4 }])

    describe "Map - string keys" do
      testDecode "{}" (Just Map.empty :: Maybe (Map.Map String Int))
      testDecode """ { "a": 1, "b": 2 } """ (Just (Map.fromFoldable [ Tuple "a" 1, Tuple "b" 2 ]))
      testDecode "1" (Nothing :: Maybe (Map.Map String Int))
      testDecode "[]" (Nothing :: Maybe (Map.Map String Int))

    describe "Map - value keys" do
      testDecode "[]" (Just Map.empty :: Maybe (Map.Map ValueKey Int))
      testDecode """ [["a", 1], ["b", 2]] """ (Just (Map.fromFoldable [ Tuple (ValueKey "a") 1, Tuple (ValueKey "b") 2 ]))
      testDecode "1" (Nothing :: Maybe (Map.Map ValueKey Int))
      testDecode "{}" (Nothing :: Maybe (Map.Map ValueKey Int))

    describe "Enum" do
      testDecode """ "A" """ (Just A)
      testDecode """ "B" """ (Just B)
      testDecode """ "C" """ (Just C)
      testDecode """ "D" """ (Nothing :: Maybe Enum)
      testDecode """ 1 """ (Nothing :: Maybe Enum)
      testDecode """ {} """ (Nothing :: Maybe Enum)

      describe "doesn't crash on undefined" do
        testDecode """ {} """ (Nothing :: Maybe { a :: Enum })

    describe "Unit" do
      testDecode """ [] """ (Just unit)
      testDecode """ "anything" """ (Just unit)

    describe "Generic" do
      describe "General sum types" do
        testDecode """ { "tag": "NoArgs" } """ (Just NoArgs)
        testDecode """ { "tag": "SingleArg", "contents": 1 } """ (Just (SingleArg 1))
        testDecode """ { "tag": "SingleRecordArg", "a": 1, "b": 2 } """ (Just (SingleRecordArg { a: 1, b: 2 }))
        testDecode """ { "tag": "ManyArgs", "contents": [{ "a": 1, "b": 2 }, 3] } """ (Just (ManyArgs { a: 1, b: 2 } 3))
        testDecode """ { "tag": "ThreeArgs", "contents": [1, 2, 3] } """ (Just (ThreeArgs 1 2 3))
        testDecode """ {} """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "SingleArg" } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "SingleRecordArg" } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "SingleRecordArg", "a": 1, "b": "junk" } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "ManyArgs" } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "ManyArgs", "contents": [] } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "ManyArgs", "contents": [{ "a": 1, "b": 2 }, 3, 4] } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "ManyArgs", "contents": [{ "a": 1, "b": 2 }] } """ (Nothing :: Maybe Sum)
        testDecode """ { "tag": "ManyArgs", "contents": [{ "a": 1, "b": "junk" }, 3] } """ (Nothing :: Maybe Sum)

        describe "doesn't crash on undefined" do
          testDecode """ {} """ (Nothing :: Maybe { a :: Sum })

      describe "Single constructor, single argument" do
        testDecode """ 1 """ (Just (SingleConstructor 1))
        testDecode """ { "a": 1 } """ (Just (SingleConstructor { a: 1 }))

      describe "Single constructor, many arguments" do
        testDecode """ ["foo", 2] """ (Just (SingleConManyArgs "foo" 2))

      describe "Sum type with polymorphic single constructor argument" do
        describe "with non-record argument" do
          testDecode """ { "tag": "C2", "contents": 1 } """ (Just (C2 1))
        describe "with record argument" do
          testDecode """ { "tag": "C2", "a": 1, "b": 2 } """ (Just (C2 { a: 1, b: 2 }))

    describe "Generic - force tagged representation" do
      describe "General sum types - should be the same as general case" do
        testDecode """ { "tag": "NoArgs" } """ (Just (ForceTagged NoArgs))
        testDecode """ { "tag": "SingleArg", "contents": 1 } """ (Just (ForceTagged (SingleArg 1)))
        testDecode """ { "tag": "SingleRecordArg", "a": 1, "b": 2 } """ (Just (ForceTagged (SingleRecordArg { a: 1, b: 2 })))
        testDecode """ {} """ (Nothing :: Maybe (ForceTagged Sum))

      describe "Single constructor, single argument - should use tagged representation" do
        testDecode """ { "tag": "SingleConstructor", "contents": 1 } """ (Just (ForceTagged (SingleConstructor 1)))
        testDecode """ { "tag": "SingleConstructor", "a": 1 } """ (Just (ForceTagged (SingleConstructor { a: 1 })))
        testDecode """ 1 """ (Nothing :: Maybe (ForceTagged (SingleConstructor Int)))
        testDecode """ { "a": 1 } """ (Nothing :: Maybe (ForceTagged (SingleConstructor { a :: Int })))

      describe "Single constructor, many arguments - should use tagged representation" do
        testDecode """ { "tag": "SingleConManyArgs", "contents": ["foo", 2] } """ (Just (ForceTagged (SingleConManyArgs "foo" 2)))
        testDecode """ ["foo", 2] """ (Nothing :: Maybe (ForceTagged SingleConManyArgs))
