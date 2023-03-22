{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.VSCodeExtensionsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.VSCodeExtensions (vsCodeExtensionsFileFor)
import Bootstrap.Data.ProjectType
  ( InstallLombok (InstallLombok),
    InstallMinishift (InstallMinishift),
    ProjectType (Java),
    SetUpJavaBuild (NoJavaBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe ".vscode/extensions.json rendering" do
  it "renders the json correctly" do
    bootstrapContent
      ( vsCodeExtensionsFileFor
          ( Java
              (InstallMinishift True)
              (InstallLombok True)
              NoJavaBuild
          )
      )
      >>= ( `shouldBe`
              Right
                [r|{
    "recommendations": [
        "arrterian.nix-env-selector",
        "jnoortheen.nix-ide",
        "vscjava.vscode-java-pack",
        "gabrielbb.vscode-lombok"
    ]
}
|]
          )
