{ name = "data-mvc"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "prelude"
  , "record"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
