-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.BuildPlanSpec (spec) where

import Bootstrap.Data.Bootstrappable.BuildNix (buildNixFor)
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc))
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.GitPodYml (gitPodYmlFor)
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.Haskell.LibHs (libHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.MainHs (mainHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.PreludeHs
  ( preludeHsFor,
  )
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShellCompat (NixShellCompat (NixShellCompat))
import Bootstrap.Data.Bootstrappable.Readme (Readme (Readme))
import Bootstrap.Data.Bootstrappable.Rust.CargoLock (cargoLockFor)
import Bootstrap.Data.Bootstrappable.Rust.CargoToml (cargoTomlFor)
import Bootstrap.Data.Bootstrappable.Rust.MainRs (mainRsFor)
import Bootstrap.Data.Bootstrappable.VSCodeExtensions (vsCodeExtensionsFileFor)
import Bootstrap.Data.Bootstrappable.VSCodeSettings (vsCodeSettingsFor)
import Bootstrap.Data.BuildPlan
  ( BuildPlan (BuildPlan),
    toBuildPlanFiles,
    toReasonTree,
  )
import Bootstrap.Data.Config (configFor)
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.HList (HList (HNil), (~:))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions), HaskellProjectType (HaskellProjectTypeBasic), ProjectType (Go, Haskell, Rust), SetUpGoBuild (SetUpGoBuild), SetUpHaskellBuild (SetUpHaskellBuild))
import Data.Tree (Tree (Node))
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.CanDieOnError ()

spec :: Spec
spec = describe "toReasonTree" do
  it "Creates the expected tree" $ do
    reasonTree <- do
      let projectName = Unsafe.fromJust $ mkProjectName "test-project"
          projectType = Go $ SetUpGoBuild True
          preCommitHooksConfig = PreCommitHooksConfig True
          ciConfig = ContinuousIntegrationConfig True
          devContainerConfig = DevContainerConfig True
          buildNix = buildNixFor projectName projectType
          haskellProjectType = Haskell $ HaskellOptions (GHCVersion 9 0 2) (HaskellProjectTypeBasic $ SetUpHaskellBuild True)
      let nixPreCommitHookConfig = nixPreCommitHookConfigFor projectType
      buildPlan <-
        BuildPlan
          <$> toBuildPlanFiles
            ( configFor projectName projectType preCommitHooksConfig ciConfig devContainerConfig
                ~: Envrc preCommitHooksConfig
                ~: buildNix
                ~: flakeNixFor projectName projectType preCommitHooksConfig (Just nixPreCommitHookConfig) buildNix
                ~: NixShellCompat
                ~: gitignoreFor projectType preCommitHooksConfig
                ~: Readme projectName projectType devContainerConfig Nothing
                ~: nixPreCommitHookConfig
                ~: gitlabCIConfigFor ciConfig projectType (Just nixPreCommitHookConfig)
                ~: devContainerDockerComposeFor devContainerConfig projectName
                ~: devContainerDockerfileFor devContainerConfig
                ~: devContainerJsonFor devContainerConfig projectName projectType
                ~: vsCodeExtensionsFileFor projectType
                ~: vsCodeSettingsFor devContainerConfig
                ~: preludeHsFor haskellProjectType
                ~: libHsFor haskellProjectType
                ~: mainHsFor haskellProjectType
                ~: cargoLockFor Rust projectName
                ~: cargoTomlFor Rust projectName
                ~: mainRsFor Rust
                ~: gitPodYmlFor projectType
                ~: HNil
            )
      pure $ toReasonTree buildPlan
    reasonTree
      `shouldBe` Node
        "/"
        [ Node
            ".devcontainer"
            [ Node "devcontainer.json - The config for the VSCode DevContainer." [],
              Node "docker-compose.yaml - The docker compose file from which the VSCode DevContainer's service is configured." [],
              Node "Dockerfile - The dockerfile from which the VSCode DevContainer is built." []
            ],
          Node ".envrc - This tells direnv to load the nix shell and set up the pre-commit hooks." [],
          Node ".gitignore - This tells git what not to track." [],
          Node ".gitlab-ci.yml - This file sets up GitLab CI." [],
          Node ".gitpod.yml - This overrides GitPod's automated tasks; they are not needed." [],
          Node ".nix-bootstrap.dhall - This holds nix-bootstrap's configuration to ensure upgrades are reliable." [],
          Node
            ".vscode"
            [ Node "extensions.json - This configures the extensions we recommend for VSCode." [],
              Node "settings.json - This configures the settings for the extensions we recommend for VSCode." []
            ],
          Node "app" [Node "Main.hs - The entrypoint of your haskell executable" []],
          Node "Cargo.lock - The locked dependencies of your rust project" [],
          Node "Cargo.toml - The configuration of your rust project" [],
          Node "flake.nix - This configures what tools are available in your development environment and links in the pre-commit hooks." [],
          Node
            "nix"
            [ Node "build.nix - This configures your reproducible project builds." [],
              Node "pre-commit-hooks.nix - This configures which pre-commit hooks are used." []
            ],
          Node "README.md - This helpfully explains to you what each file (including itself) does!" [],
          Node "shell.nix - This enables you to use your development shell when Nix flakes aren't available." [],
          Node
            "src"
            [ Node "Lib.hs - The entrypoint of your haskell library" [],
              Node "main.rs - Your rust application's entrypoint" [],
              Node "Prelude.hs - The haskell prelude - what this exports is implicitly imported into every other module" []
            ]
        ]
