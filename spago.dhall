{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "prognosticator"
, dependencies =
    [ "aff"
    , "affjax"
    , "arrays"
    , "console"
    , "effect"
    , "either"
    , "express"
    , "foldable-traversable"
    , "foreign"
    , "foreign-generic"
    , "functions"
    , "generics-rep"
    , "halogen"
    , "halogen-css"
    , "halogen-vdom"
    , "integers"
    , "js-date"
    , "maybe"
    , "node-postgres"
    , "node-process"
    , "now"
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "refs"
    , "routing"
    , "simple-json"
    , "strings"
    , "transformers"
    , "tuples"
    , "type-equality"
    , "web-html"
    ]
, packages =
    ./packages.dhall
}
