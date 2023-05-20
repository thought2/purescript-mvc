{ name = "data-mvc"
, dependencies =
  [ "prelude"
  , "record"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
