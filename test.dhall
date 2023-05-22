let conf = ./spago.dhall
in conf // {
  sources = conf.sources # [ "test/**/*.purs", "sample/**/*.purs" ],
  dependencies = conf.dependencies # 
    [ "aff"
    , "effect"
    , "spec"
    , "typelevel-prelude"
    , "console"
    , "virtual-dom"
    , "halogen"
    , "halogen-vdom"
    , "virtual-dom-halogen"
    , "integers"
    , "maybe"
    ]
}