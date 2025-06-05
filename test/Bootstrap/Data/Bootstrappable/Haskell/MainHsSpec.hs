{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.MainHsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Haskell.MainHs (mainHsFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeServer),
    ProjectType (Haskell),
    SetUpHaskellBuild (SetUpHaskellBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Main.hs rendering" do
  it "renders correctly for a simple library" do
    bootstrapContent (mainHsFor . Haskell $ HaskellOptions (GHCVersion 9 0 2) (HaskellProjectTypeBasic $ SetUpHaskellBuild True))
      >>= ( `shouldBe`
              Right
                [r|module Main (main) where

import Lib (lib)

main :: IO ()
main = lib
|]
          )
  it "renders correctly for a WAI app served by warp" do
    bootstrapContent (mainHsFor . Haskell $ HaskellOptions (GHCVersion 9 0 2) (HaskellProjectTypeServer $ SetUpHaskellBuild True))
      >>= ( `shouldBe`
              Right
                [r|module Main (main) where

import Network.Wai.Handler.Warp (run)
import Server (app)

main :: IO ()
main = do {putTextLn "Serving on 8080..."; run 8080 app}
|]
          )
