{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.PackageJsonSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.PackageJson (elmPackageJsonFor)
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
spec = describe "package.json rendering" do
  it "renders the package.json correctly" do
    bootstrapContent (elmPackageJsonFor . Elm $ ElmOptions (ElmModeNode NPM) True)
      >>= ( `shouldBe`
              Right
                [r|{
    "dependencies": {
        "buffer": "^5.5.0",
        "process": "^0.11.10",
        "punycode": "^1.4.1",
        "querystring-es3": "^0.2.1",
        "url": "^0.11.0"
    },
    "devDependencies": {
        "@babel/core": ">=7.13.0 <8.0.0",
        "@babel/preset-env": "^7.1.6",
        "@parcel/core": ">=2.8.3 <3.0.0",
        "@parcel/transformer-elm": "^2.8.3",
        "elm": "^0.19.1-5",
        "parcel": "^2.8.3"
    },
    "peerDependencies": {
        "@babel/core": ">=7.13.0 <8.0.0",
        "@babel/preset-env": "^7.1.6",
        "@parcel/core": ">=2.8.3 <3.0.0",
        "elm": "^0.19.1-5"
    },
    "scripts": {
        "build": "NODE_ENV=production parcel build",
        "dev": "NODE_ENV=development parcel"
    },
    "source": "src/index.html"
}
|]
          )
