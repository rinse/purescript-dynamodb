{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
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
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
