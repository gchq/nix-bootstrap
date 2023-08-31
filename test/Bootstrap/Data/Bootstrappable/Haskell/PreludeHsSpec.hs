{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.PreludeHsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Haskell.PreludeHs (preludeHsFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic),
    ProjectType (Haskell),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Prelude.hs rendering" do
  it "renders correctly" do
    bootstrapContent (preludeHsFor . Haskell $ HaskellOptions (GHCVersion 9 0 2) HaskellProjectTypeBasic)
      >>= ( `shouldBe`
              Right
                [r|{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Prelude (module Relude) where

import Relude
|]
          )
