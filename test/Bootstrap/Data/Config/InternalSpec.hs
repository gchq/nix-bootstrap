{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Config.InternalSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Config (configFor)
import Bootstrap.Data.ContinuousIntegration (ContinuousIntegrationConfig (ContinuousIntegrationConfig))
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType (NodePackageManager (PNPm), ProjectType (Node))
import Bootstrap.Data.ProjectTypeSpec ()
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe ".nix-bootstrap.toml rendering" do
  it "renders correctly" do
    let projectName = Unsafe.fromJust $ mkProjectName "test-project"
    bootstrapContent
      ( configFor
          projectName
          (Node PNPm)
          (PreCommitHooksConfig True)
          (ContinuousIntegrationConfig True)
          (DevContainerConfig True)
          False
      )
      >>= ( `shouldBe`
              Right
                [r|-- This file was generated by nix-bootstrap.
-- It should be checked into version control.
-- It is used to aid migration between nix-bootstrap versions and preserve idempotence.

let JavaOptions =
      { installMinishift : Bool
      , installLombok : Bool
      , setUpJavaBuild : < SetUpJavaBuild : Text | NoJavaBuild >
      }

let ProjectType =
      < Minimal
      | Node : < NPM | PNPm | Yarn >
      | Go : Bool
      | Java : JavaOptions
      | Python
      >

in  { projectName = "test-project"
    , projectType = ProjectType.Node < NPM | PNPm | Yarn >.PNPm
    , setUpPreCommitHooks = True
    , setUpContinuousIntegration = True
    , setUpVSCodeDevContainer = True
    , useNixFlakes = False
    }
|]
          )