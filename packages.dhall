let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220523/packages.dhall
        sha256:985f90fa68fd8b43b14c777d6ec2c161c4dd9009563b6f51685a54e4a26bf8ff

in  upstream
  with spec-mocha =
    { dependencies =
      [ "aff"
      , "console"
      , "datetime"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "maybe"
      , "newtype"
      , "prelude"
      , "spec"
      ]
    , repo = "https://github.com/restaumatic/purescript-spec-mocha.git"
    , version = "v4.0.0-restaumatic2"
    }
  with foreign-generic =
    { dependencies =
      [ "effect"
      , "foreign"
      , "foreign-object"
      , "ordered-collections"
      , "exceptions"
      , "record"
      ]
    , repo =
        "https://github.com/working-group-purescript-es/purescript-foreign-generic.git"
    , version = "e7fa22dc9fc2351485f2e915fa7d418ca1965c6d"
    }
  with simple-json =
    { dependencies =
      [ "prelude"
      , "typelevel-prelude"
      , "record"
      , "variant"
      , "nullable"
      , "foreign-object"
      , "foreign"
      , "exceptions"
      , "arrays"
      ]
    , repo = "https://github.com/justinwoo/purescript-simple-json.git"
    , version = "b85e112131240ff95b5c26e9abb8e2fa6db3c656"
    }
