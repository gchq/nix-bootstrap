{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitignoreSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectType
  ( NodePackageManager (Yarn),
    ProjectType (Go, Node),
    SetUpGoBuild (SetUpGoBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.RunConfig (rcDefault, rcWithFlakes)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe ".gitignore rendering" do
  it "renders blocks correctly with go and flakes, without pre-commit hooks" do
    bootstrapContent (gitignoreFor rcWithFlakes (Go $ SetUpGoBuild False) (PreCommitHooksConfig False))
      >>= ( `shouldBe`
              Right
                [r|# Binaries for programs and plugins
*.exe
*.exe~
*.dll
*.so
*.dylib

# Direnv Config
/.direnv

# Go workspace file
go.work

# Nix Artefacts
result
result-*

# OS-Specific
.DS_Store
*~

# Output of go coverage tool
*.out

# Test binary, built with `go test -c`
*.test
|]
          )

  it "renders blocks correctly with yarn and pre-commit hooks" do
    bootstrapContent (gitignoreFor rcDefault (Node Yarn) (PreCommitHooksConfig True))
      >>= ( `shouldBe`
              Right
                [r|# Nix Artefacts
result
result-*

# Node
/node_modules

# OS-Specific
.DS_Store
*~

# Pre-Commit Hooks
/.pre-commit-config.yaml

# Yarn error log
yarn-error.log
|]
          )
