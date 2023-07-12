{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ProjectTypeSpec (spec) where

import Bootstrap.Data.ProjectType
  ( ArtefactId (ArtefactId),
    ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (ElmOptions),
    JavaOptions (JavaOptions),
    ProjectType (Elm, Go, Java, Minimal, Node, Python),
    SetUpJavaBuild (NoJavaBuild, SetUpJavaBuild),
  )
import Data.Char (isAsciiLower, isAsciiUpper)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    arbitraryBoundedEnum,
    choose,
    oneof,
    suchThat,
    vectorOf,
  )
import Test.Util (dhallRoundtripTest)

instance Arbitrary ProjectType where
  arbitrary =
    oneof
      [ pure Minimal,
        Elm
          <$> ( ElmOptions
                  <$> oneof [pure ElmModeBare, ElmModeNode <$> arbitraryBoundedEnum]
                  <*> arbitrary
              ),
        Node <$> arbitraryBoundedEnum,
        Go <$> arbitraryBoundedEnum,
        Java <$> (JavaOptions <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum <*> arbitrary),
        Python <$> arbitraryBoundedEnum
      ]

instance Arbitrary SetUpJavaBuild where
  arbitrary =
    oneof
      [ SetUpJavaBuild <$> arbitrary,
        pure NoJavaBuild
      ]

instance Arbitrary ArtefactId where
  arbitrary = do
    l <- choose (1, 10)
    artefactIdChars <- vectorOf l (arbitrary `suchThat` \c -> isAsciiUpper c || isAsciiLower c)
    pure . ArtefactId $ toText artefactIdChars

spec :: Spec
spec = describe "ProjectType" do
  prop "roundtrips to Dhall" (dhallRoundtripTest @ProjectType)
