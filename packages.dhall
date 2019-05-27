let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190508/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190508/src/packages.dhall sha256:8ef3a6d6d123e05933997426da68ef07289e1cbbdd2a844b5d10c9159deef65a

let overrides =
  { halogen =
      upstream.halogen 
    // { repo = "https://github.com/slamdata/purescript-halogen"
        , version = "v5.0.0-rc.4"
        }
  , halogen-vdom =
      upstream.halogen-vdom
    // { repo = "https://github.com/slamdata/purescript-halogen-vdom"
        , version = "v6.1.0"
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
