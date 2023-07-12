{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.ElmJson
  ( ElmJson,
    elmJsonFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.ProjectType (ProjectType (Elm))
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.QQ.Simple (aesonQQ)

data ElmJson = ElmJson

instance Bootstrappable ElmJson where
  bootstrapName = const "elm.json"
  bootstrapReason = const "The configuration of your elm project"
  bootstrapContent = bootstrapContentPrettyJson ["type", "source-directories", "elm-version"]

instance ToJSON ElmJson where
  toJSON ElmJson =
    [aesonQQ|{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
|]

elmJsonFor :: ProjectType -> Maybe ElmJson
elmJsonFor = \case
  Elm _ -> Just ElmJson
  _ -> Nothing
