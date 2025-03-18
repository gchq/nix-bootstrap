{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.Review.ElmJson
  ( ElmReviewElmJson,
    elmReviewElmJsonFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.ProjectType
  ( ElmOptions (ElmOptions, elmOptionElmMode, elmOptionProvideElmReview),
    ProjectType (Elm),
  )
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.QQ.Simple (aesonQQ)

data ElmReviewElmJson = ElmReviewElmJson

instance Bootstrappable ElmReviewElmJson where
  bootstrapName = const "review/elm.json"
  bootstrapReason = const "The configuration of the dependencies of the elm-review tool"
  bootstrapContent = bootstrapContentPrettyJson ["type", "source-directories", "elm-version"]

instance ToJSON ElmReviewElmJson where
  toJSON ElmReviewElmJson =
    [aesonQQ|
{
  "type": "application",
  "source-directories": ["src"],
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
      "elm/bytes": "1.0.8",
      "elm/html": "1.0.0",
      "elm/json": "1.1.3",
      "elm/parser": "1.1.0",
      "elm/project-metadata-utils": "1.0.2",
      "elm/random": "1.0.0",
      "elm/time": "1.0.0",
      "elm/virtual-dom": "1.0.3",
      "elm-community/list-extra": "8.7.0",
      "elm-explorations/test": "2.1.1",
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

elmReviewElmJsonFor :: ProjectType -> Maybe ElmReviewElmJson
elmReviewElmJsonFor (Elm ElmOptions {..})
  | elmOptionProvideElmReview =
      Just ElmReviewElmJson
elmReviewElmJsonFor _ = Nothing
