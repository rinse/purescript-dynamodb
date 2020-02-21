# purescript-dynamodb

Dynamodb bindings for purescript.

## installiation

1. Add the following addtional repos to your `packages.dhall`.

```purescript
let additions =
    { aws-core =
        { dependencies = [ "console", "effect", "psci-support", "spec-discovery" ]
        , repo = "https://github.com/rinse/purescript-aws-core.git"
        , version = "master"
        }
    , dynamodb =
        { dependencies =
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
        , repo = "https://github.com/rinse/purescript-dynamodb.git"
        , version = "master"
        }
    }
```

2. Install `aws-sdk`.

```bash
$ npm install --save-dev aws-sdk
```
