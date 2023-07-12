{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.Review.ElmJsonSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.Review.ElmJson (elmReviewElmJsonFor)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare),
    ElmOptions (ElmOptions),
    ProjectType (Elm),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "elm-review elm.json rendering" do
  it "renders the elm-review elm.json correctly" do
    bootstrapContent (elmReviewElmJsonFor . Elm $ ElmOptions ElmModeBare True)
      >>= ( `shouldBe`
              Right
                [r|{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "Arkham/elm-review-no-missing-type-constructor": "1.0.2",
            "elm/core": "1.0.5",
            "jfmengels/elm-review": "2.12.2",
            "jfmengels/elm-review-code-style": "1.1.3",
            "jfmengels/elm-review-common": "1.3.2",
            "jfmengels/elm-review-simplify": "2.0.28",
            "jfmengels/elm-review-the-elm-architecture": "1.0.3",
            "jfmengels/elm-review-unused": "1.1.29",
            "sparksp/elm-review-ports": "1.3.1",
            "stil4m/elm-syntax": "7.2.9",
            "truqu/elm-review-nobooleancase": "1.0.1"
        },
        "indirect": {
            "elm-community/list-extra": "8.7.0",
            "elm-explorations/test": "2.1.1",
            "elm/bytes": "1.0.8",
            "elm/html": "1.0.0",
            "elm/json": "1.1.3",
            "elm/parser": "1.1.0",
            "elm/project-metadata-utils": "1.0.2",
            "elm/random": "1.0.0",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.3",
            "miniBill/elm-unicode": "1.0.3",
            "pzp1997/assoc-list": "1.0.0",
            "rtfeldman/elm-hex": "1.0.0",
            "stil4m/structured-writer": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm-explorations/test": "2.1.1"
        },
        "indirect": {}
    }
}
|]
          )
