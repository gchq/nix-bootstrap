{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.IndexJsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.IndexJs (elmIndexJsFor)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeNode),
    ElmOptions (ElmOptions),
    NodePackageManager (NPM),
    ProjectType (Elm),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "index.js rendering" do
  it "renders the index.js correctly" do
    bootstrapContent (elmIndexJsFor . Elm $ ElmOptions (ElmModeNode NPM) True)
      >>= ( `shouldBe`
              Right
                [r|import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("root"),
});
|]
          )
