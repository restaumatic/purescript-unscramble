# Unscramble [![CircleCI](https://circleci.com/gh/restaumatic/purescript-unscramble/tree/master.svg?style=svg)](https://circleci.com/gh/restaumatic/purescript-unscramble/tree/master)

**Unscramble** ("decode while visibly struggling") - a hopefully faster JSON decoding library for PureScript.

**Warning**: not stable yet, the API will probably change.

## Why

Decoding data is often on the critical path of loading a page. Other PureScript decoding libraries have too much overhead for some use cases.

## Usage

This library offers typeclass-based conversion from JSON (in the form of already parsed JavaScript objects, `Data.Foreign`) to arbitrary PureScript data types.

Note that it doesn't offer encoding. You have to use some other library to do that.

The encoding format is compatible with Haskell `aeson` generic encoding with default options. Note: this means it's not fully compatible with `foreign-generic`. See [Data type encoding](#Data_type_encoding) below for details.

The main entry points are:

```purescript
module Unscramble where

type DecodingError = String
type Result = Either DecodingError

decode :: forall a. Decode a => Foreign -> Maybe a

decodeEither :: forall a. Decode a => Foreign -> Result a

decodeJSON :: forall a. Decode a => String -> Maybe a

decodeJSONEither :: forall a. Decode a => String -> Result a
```

To use it on your own data types, you need to obtain an instance of the `Decode` class.

```purescript
class Decode a where
  -- WARNING: Partial function, may throw a `DecodingError` JavaScript exception.
  unsafeDecode :: Foreign -> a
```

If you are generating your types in some way, the fastest option would be to modify the generator to produce a "hand-written" instance.
Otherwise, use the generic options below.

### Records

There is an instance for `Record`. Records are encoded, obviously, as JavaScript objects.

If you have a nominal product type (a `Record` wrapper), just use newtype deriving:

```purescript
import Unscramble (class Decode)

newtype Person = Person { name :: String, age :: Int }

derive newtype instance decodePerson :: Decode Person
```

### Sum types

The easiest way is to use generic deriving:

```purescript
import Data.Generic.Rep (class Generic)
import Unscramble (class Decode)
import Unscramble.Generic as UG

data Person = NamedPerson { name :: String } | NamelessPerson

derive instance genericPerson :: Generic Person _
instance decodePerson :: Decode Person where
  unsafeDecode = UG.unsafeGenericDecode UG.defaultOptions
```

Note: The partial application of `unsafeGenericDecode` precomputes a lookup table for constructor tags.
To avoid reconstructing it on every decode, make sure it is not behind a lambda. For example:

```purescript
instance decodePerson :: Decode Person where
  -- GOOD, `UG.unsafeGenericDecode UG.defaultOptions` is computed once
  unsafeDecode = someFixup <<< UG.unsafeGenericDecode UG.defaultOptions
    where someFixup person = ...

instance decodePerson :: Decode Person where
  -- BAD, `UG.unsafeGenericDecode UG.defaultOptions` is computed for every decoded value
  unsafeDecode value = someFixup (UG.unsafeGenericDecode UG.defaultOptions value)
    where someFixup person = ...
```

If you need more complicated logic than a simple function composition, make the sharing explicit, e.g.:

```purescript
instance decodePerson :: Decode Person where
  -- GOOD, `UG.unsafeGenericDecode UG.defaultOptions` is computed once
  unsafeDecode =
    let decode = UG.unsafeGenericDecode UG.defaultOptions
    in \value ->
      if U.isString value then
        ... some special case ...
      else
        decode value
```

### Enums

As a special case, sum types with no constructor arguments can use the string representation (value encoded as the constructor name).

```purescript
import Data.Generic.Rep (class Generic)
import Unscramble (class Decode)
import Unscramble.Enum as UE

data Weather = Sunny | Rainy | Cold

derive instance genericWeather :: Generic Weather _
instance decodeWeather :: Decode Weather where
  unsafeDecode = UE.unsafeGenericDecodeEnum UE.defaultEnumOptions
```

### Custom types

You may want to, for example, provide an UUID type which is encoded as String but requires additional validation.

Use `unsafeDecode` to decode to the original representation. This library uses JavaScript exceptions for error reporting, so the function is partial. But the implementation you're writing also is, so that's OK. See [How it works](#How_it_works) below for details.

After obtaining the value, you can perform additional validation, and use `decodingError :: forall a. String -> a` (also, obviously, partial) to indicate failure.

Example:

```purescript
import Unscramble as U

newtype UUID = UUID String

instance decodeUUID :: Decode UUID where
  unsafeDecode value =
    let str = U.unsafeDecode value :: String in
    if isValidUUID str then
      UUID str
    else 
      U.decodingError "Invalid UUID"

isValidUUID :: String -> Boolean
isValidUUID = {- ... check if the format matches ... -}
```

#### Multiple alternatives

You may want to accept multiple representations (e.g. a number as String or as a number).

Don't use backtracking for that - it would result in wasted work to generate the error message in the failed branch.
Instead, use helper functions to decide which branch to take, and commit to it.

```purescript
Unscramble.isString :: Foreign -> Boolean
Unscramble.isNumber :: Foreign -> Boolean
Foreign.isNull :: Foreign -> Boolean
Foreign.isUndefined :: Foreign -> Boolean
Foreign.isArray :: Foreign -> Boolean
```

(Note: some are missing, e.g. `isObject`; if you need it, please file an issue or PR)

Example:

```purescript
import Data.Number as Number
import Unscramble as U

newtype Num = Num Number

-- | `Num` can be encoded either as JSON Number, or JSON String.
instance decodeNum :: U.Decode Num where
  unsafeDecode value =
    if U.isString value then
      case Number.fromString (U.unsafeDecode value) of
        Just n -> Num n
        Nothing -> U.decodingError "Invalid number"
    else
      Num (U.unsafeDecode value)
```

## How it works

`unscramble` is built on the following principles:

- The goal of decoding is to succeed, not fail.
  - The success code path should be as fast as possible.
  - Decoding failure is a rare occurence caused by programmer error. The behavior of the library in this case (i.e. the error messages) should be only as good so that the developer can fix the problem.
    - For example, it would be perfectly acceptable to have _no_ error messages at at all, if it improved performance in the success case. In case of decoding failure, the developer could always reproduce the issue locally with a modified version of the library which does report errors.
- The target language (JavaScript) should be utilized properly, even if it means sacrificing some ideals of the source language (PureScript), or even some readability.
  - For example: prefer loops over recursion.

The architecture of `unscramble` follows from these principles:

- We don't use monads (`Maybe/Either`) for representing failure, because they compile to non-idiomatic JavaScript and slow down the success path. Instead, JavaScript exceptions are used for reporting failure.
- This makes the decoding functions partial. Despite that, there is no `Partial` constraint on them, because it would add another level of currying, therefore adding more function allocations, destroying sharing, and overall resulting in slowdown of the code.

### Is it worth it?

Should we sacrifice functional purity for performance gains?

```
$ npm run bench-micro "R10"
...
Filters: ["R10"]
R10 Unscramble                          : 0.000514ms/op
R10 Argonaut                            : 0.007123ms/op
R10 Foreign.Generic                     : 0.013110ms/op
R10 Simple.JSON                         : 0.013957ms/op
...
```

(R10 means "decoding a `Record` with 10 fields")

14x speedup over Argonaut, 26x speedup over Foreign.Generic and Simple.JSON. It is at least worth considering :).

See [doc/Performance.md](./doc/Performance.md) for more details.

### Why not use `Effect`? It has support for JS exceptions

This is an additional layer of indirection on top of JavaScript. Which in _most_ cases can be eliminated thanks to `MagicDo` and `EffectFn...`, but not always.

## Stability

The library is new, the API will probably change. Most likely we'll make the primitive decoding functions look more scary by adding words like `unsafe`, `internal`, `don't` or `php`.

## Contributions

All improvements are welcome, especially:
- documentation
- performance
- better benchmarking (more realistic benchmarks, comparison with more alternatives etc.).

## Contact

If you discover bugs, want new features, or have questions, please post an issue using the GitHub issue tracker.
Alternatively you can use the GitHub discussion feature (enabled in this repo).
