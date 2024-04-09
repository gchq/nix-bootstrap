{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.IndexHtmlSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.IndexHtml
  ( elmIndexHtmlFor,
  )
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeNode),
    ElmOptions (ElmOptions),
    NodePackageManager (NPM),
    ProjectType (Elm),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "index.html rendering" do
  it "renders the index.html correctly" do
    bootstrapContent
      ( elmIndexHtmlFor (Unsafe.fromJust $ mkProjectName "test-project")
          . Elm
          $ ElmOptions (ElmModeNode NPM) True
      )
      >>= ( `shouldBe`
              Right
                [r|<!DOCTYPE html>

<html>
    <head>
        <meta charset="utf-8" />
        <title>
            test-project
        </title>
        <script type="module" src="./index.js">
        </script>
    </head>
    <body>
        <div id="root">
        </div>
    </body>
</html>
|]
          )
