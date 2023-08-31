{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.PackageYamlSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Haskell.PackageYaml (packageYamlFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic),
    ProjectType (Haskell),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "package.yaml rendering" do
  it "renders correctly" do
    let projectName = Unsafe.fromJust $ mkProjectName "test-project"
    bootstrapContent (packageYamlFor (Haskell $ HaskellOptions (GHCVersion 9 0 2) HaskellProjectTypeBasic) projectName)
      >>= ( `shouldBe`
              Right
                [r|default-extensions:
- BlockArguments
- DerivingStrategies
- GADTs
- LambdaCase
- NamedFieldPuns
- OverloadedStrings
- RecordWildCards
- StrictData
dependencies:
- mixin:
  - hiding (Prelude)
  name: base
- relude
executables:
  app:
    dependencies: test-project
    ghc-options:
    - -O2
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    main: Main.hs
    source-dirs: app
flags:
  prod:
    default: false
    description: Enable production defaults
    manual: true
ghc-options:
- -Wall
- -Wcpp-undef
- -Widentities
- -Wincomplete-patterns
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-deriving-strategies
- -Wmissing-export-lists
- -Wmissing-import-lists
- -Wmissing-signatures
- -Wpartial-fields
- -Wredundant-constraints
library:
  dependencies: []
  source-dirs: src
name: test-project
version: 0.1.0.0
when:
- condition: flag(prod)
  ghc-options:
  - -O2
  - -Werror
|]
          )
