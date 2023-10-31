{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitPodYmlSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.GitPodYml (gitPodYmlFor)
import Bootstrap.Data.ProjectType
  ( NodePackageManager (NPM),
    ProjectType (Node),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe ".gitpod.yml rendering" do
  it "renders the yml correctly" do
    bootstrapContent (gitPodYmlFor (Node NPM))
      >>= ( `shouldBe`
              Right
                [r|tasks: []
vscode:
  extensions:
  - arrterian.nix-env-selector
  - jnoortheen.nix-ide
|]
          )
