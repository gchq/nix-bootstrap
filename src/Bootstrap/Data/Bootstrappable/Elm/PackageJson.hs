{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.PackageJson
  ( ElmPackageJson,
    elmPackageJsonFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.ProjectType (ElmMode (ElmModeNode), ElmOptions (ElmOptions), ProjectType (Elm))
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.QQ.Simple (aesonQQ)

data ElmPackageJson = ElmPackageJson

instance Bootstrappable ElmPackageJson where
  bootstrapName = const "package.json"
  bootstrapReason = const "The node configuration of your Elm project"
  bootstrapContent = bootstrapContentPrettyJson []

instance ToJSON ElmPackageJson where
  toJSON ElmPackageJson =
    [aesonQQ|{
  "devDependencies": {
    "@babel/core": ">=7.13.0 <8.0.0",
    "@babel/preset-env": "^7.1.6",
    "@parcel/core": ">=2.8.3 <3.0.0",
    "@parcel/transformer-elm": "^2.8.3",
    "elm": "^0.19.1-5",
    "parcel": "^2.8.3"
  },
  "scripts": {
    "dev": "NODE_ENV=development parcel",
    "build": "NODE_ENV=production parcel build"
  },
  "source": "src/index.html",
  "peerDependencies": {
    "@babel/core": ">=7.13.0 <8.0.0",
    "@babel/preset-env": "^7.1.6",
    "@parcel/core": ">=2.8.3 <3.0.0",
    "elm": "^0.19.1-5"
  },
  "dependencies": {
    "buffer": "^5.5.0",
    "process": "^0.11.10",
    "punycode": "^1.4.1",
    "querystring-es3": "^0.2.1",
    "url": "^0.11.0"
  }
}
|]

elmPackageJsonFor :: ProjectType -> Maybe ElmPackageJson
elmPackageJsonFor = \case
  Elm (ElmOptions (ElmModeNode _) _) -> Just ElmPackageJson
  _ -> Nothing
