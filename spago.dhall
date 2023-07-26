{ name = "mvc"
, dependencies =
  [ "heterogeneous", "maybe", "newtype", "prelude", "record", "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
