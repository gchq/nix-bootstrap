-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.VersionSpec (spec) where

import Bootstrap.Data.Version (toMajorVersion)
import qualified Data.Version as V
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "MajorVersion's Ord instance" $
  it "orders versions correctly" do
    let mkVersion = Unsafe.fromJust . toMajorVersion . V.makeVersion
        a = mkVersion [0, 1, 0]
        b = mkVersion [1, 0, 0]
        c = mkVersion [1, 0, 1]
        d = mkVersion [2, 0, 1]
    a `shouldSatisfy` (<= b)
    b `shouldSatisfy` (<= c)
    c `shouldSatisfy` (<= d)
    a `shouldSatisfy` (<= d)
    a `shouldSatisfy` (<= a)
