let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190508/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.6-20200404/src/packages.dhall sha256:f239f2e215d0cbd5c203307701748581938f74c4c78f4aeffa32c11c131ef7b6

let overrides =
      { halogen =
              upstream.halogen
          //  { repo = "https://github.com/slamdata/purescript-halogen"
              , version = "v5.0.0-rc.7"
              }
      , halogen-vdom =
              upstream.halogen-vdom
          //  { repo = "https://github.com/slamdata/purescript-halogen-vdom"
              , version = "v6.1.3"
              }
      }

let additions =
      { express =
          mkPackage
            [ "aff", "foreign", "foreign-generic", "node-http", "test-unit" ]
            "https://github.com/nkly/purescript-express.git"
            "v0.8.0"
      }

in  upstream // overrides // additions
