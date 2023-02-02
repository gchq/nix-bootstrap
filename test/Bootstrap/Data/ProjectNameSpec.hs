-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ProjectNameSpec (spec) where

import Bootstrap.Data.ProjectName
  ( ProjectName (unProjectName),
    mkProjectName,
    replaceSpacesWithDashes,
    tomlBiMap,
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Toml

spec :: Spec
spec = describe "ProjectName" do
  describe "mkProjectName" do
    it "Does not remove spaces" do
      unProjectName (Unsafe.fromJust $ mkProjectName "my Project") `shouldBe` "my Project"
  describe "replaceSpacesWithDashes" do
    it "Replaces spaces with dashes" do
      replaceSpacesWithDashes <$> mkProjectName "my Project" `shouldBe` mkProjectName "my-Project"
  describe "tomlBiMap" do
    it "Converts valid project names bidirectionally" do
      let Toml.BiMap {..} = tomlBiMap
          projectName = Unsafe.fromJust $ mkProjectName "my-Project"
      forward projectName `shouldBe` Right (Toml.AnyValue (Toml.Text "my-Project"))
      backward (Toml.AnyValue (Toml.Text "my-Project")) `shouldBe` Right projectName
    it "Fails if the serialised project name is invalid" do
      let Toml.BiMap {..} = tomlBiMap
      backward (Toml.AnyValue (Toml.Text "1y-Project")) `shouldBe` Left (Toml.ArbitraryError "Invalid project name")
