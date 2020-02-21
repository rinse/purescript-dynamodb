{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dynamodb"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "aws-core"
    , "console"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "profunctor"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
