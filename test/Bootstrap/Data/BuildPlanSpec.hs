-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.BuildPlanSpec (spec) where

import Bootstrap.Data.Bootstrappable.BuildNix (buildNixFor)
import Bootstrap.Data.Bootstrappable.DefaultNix (defaultNixFor)
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc))
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml (GitPodYml))
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.Haskell.LibHs (libHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.MainHs (mainHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.PackageYaml
  ( packageYamlFor,
  )
import Bootstrap.Data.Bootstrappable.Haskell.PreludeHs
  ( preludeHsFor,
  )
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.Bootstrappable.NixShellCompat (nixShellCompatFor)
import Bootstrap.Data.Bootstrappable.Readme (Readme (Readme))
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
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions), HaskellProjectType (HaskellProjectTypeBasic), ProjectType (Go, Haskell), SetUpGoBuild (SetUpGoBuild))
import Data.Tree (Tree (Node))
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.CanDieOnError ()
import Test.Util.RunConfig (rcDefault, rcWithFlakes)

spec :: Spec
spec = describe "toReasonTree" do
  it "Creates the expected tree" $ do
    reasonTree <- do
      let projectName = Unsafe.fromJust $ mkProjectName "test-project"
          projectType = Go $ SetUpGoBuild True
          preCommitHooksConfig = PreCommitHooksConfig True
          ciConfig = ContinuousIntegrationConfig True
          devContainerConfig = DevContainerConfig True
          buildNix = buildNixFor rcWithFlakes projectName projectType
          haskellProjectType = Haskell $ HaskellOptions (GHCVersion 9 0 2) HaskellProjectTypeBasic
      let nixPreCommitHookConfig = nixPreCommitHookConfigFor rcDefault projectType
      buildPlan <-
        BuildPlan
          <$> toBuildPlanFiles
            ( configFor projectName projectType preCommitHooksConfig ciConfig devContainerConfig False
                ~: Envrc preCommitHooksConfig False
                ~: buildNix
                ~: flakeNixFor rcWithFlakes projectName projectType preCommitHooksConfig (Just nixPreCommitHookConfig) buildNix
                ~: defaultNixFor projectName projectType
                ~: nixShellFor rcDefault projectType preCommitHooksConfig (Just nixPreCommitHookConfig)
                ~: nixShellCompatFor rcWithFlakes
                ~: gitignoreFor rcDefault projectType preCommitHooksConfig
                ~: Readme projectName projectType devContainerConfig Nothing False
                ~: nixPreCommitHookConfig
                ~: gitlabCIConfigFor ciConfig rcDefault projectType preCommitHooksConfig
                ~: devContainerDockerComposeFor devContainerConfig projectName
                ~: devContainerDockerfileFor devContainerConfig
                ~: devContainerJsonFor devContainerConfig projectName projectType
                ~: vsCodeExtensionsFileFor projectType
                ~: vsCodeSettingsFor devContainerConfig
                ~: packageYamlFor haskellProjectType projectName
                ~: preludeHsFor haskellProjectType
                ~: libHsFor haskellProjectType
                ~: mainHsFor haskellProjectType
                ~: GitPodYml
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
          Node "default.nix - This configures your reproducible project builds." [],
          Node "flake.nix - This configures what tools are available in your development environment and links in the pre-commit hooks." [],
          Node
            "nix"
            [ Node "build.nix - This configures your reproducible project builds." [],
              Node "pre-commit-hooks.nix - This configures which pre-commit hooks are used." [],
              Node "sources.json - This contains metadata about your nix dependencies." [],
              Node "sources.nix - This is the interface between nix and the dependencies listed in sources.json." []
            ],
          Node "package.yaml - The configuration of your haskell project" [],
          Node "README.md - This helpfully explains to you what each file (including itself) does!" [],
          Node "shell.nix - This configures what tools are available in your development environment and links in the pre-commit hooks." [],
          Node "shell.nix - This enables you to use your development shell when Nix flakes aren't available." [],
          Node
            "src"
            [ Node "Lib.hs - The entrypoint of your haskell library" [],
              Node "Prelude.hs - The haskell prelude - what this exports is implicitly imported into every other module" []
            ]
        ]
