{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-D3-tagless"
, dependencies =
    [ "effect"
    , "console"
    , "psci-support"
    , "lists"
    , "integers"
    , "nullable"
    , "ordered-collections"
    , "profunctor"
    , "transformers"
    , "web-events"
    , "web-html"
    , "colors"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
