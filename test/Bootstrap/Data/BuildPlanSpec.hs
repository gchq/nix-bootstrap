-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.BuildPlanSpec (spec) where

import Bootstrap.Data.Bootstrappable.BootstrapState
  ( bootstrapStateFor,
  )
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc))
import Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml (GitPodYml))
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.Bootstrappable.Readme (Readme (Readme))
import Bootstrap.Data.Bootstrappable.VSCodeSettings (vsCodeSettingsFor)
import Bootstrap.Data.BuildPlan
  ( BuildPlan (BuildPlan),
    toBuildPlanFiles,
    toReasonTree,
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Data.HList (HList (HNil), (~:))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType (ProjectType (Go), SetUpGoBuild (SetUpGoBuild))
import Data.Tree (Tree (Node))
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.CanDieOnError ()
import Test.Util.RunConfig (rcDefault)

spec :: Spec
spec = describe "toReasonTree" do
  it "Creates the expected tree" $ do
    reasonTree <- do
      let projectName = Unsafe.fromJust $ mkProjectName "test-project"
          projectType = Go $ SetUpGoBuild False
          preCommitHooksConfig = PreCommitHooksConfig True
          ciConfig = ContinuousIntegrationConfig True
          devContainerConfig = DevContainerConfig True
      let nixPreCommitHookConfig = nixPreCommitHookConfigFor rcDefault projectType
      buildPlan <-
        BuildPlan
          <$> toBuildPlanFiles
            ( bootstrapStateFor projectName projectType preCommitHooksConfig ciConfig devContainerConfig False
                ~: Envrc preCommitHooksConfig False
                ~: nixShellFor rcDefault projectType preCommitHooksConfig (Just nixPreCommitHookConfig)
                ~: gitignoreFor rcDefault projectType preCommitHooksConfig
                ~: Readme projectName projectType devContainerConfig Nothing False
                ~: nixPreCommitHookConfig
                ~: gitlabCIConfigFor ciConfig rcDefault projectType preCommitHooksConfig
                ~: devContainerDockerComposeFor devContainerConfig projectName
                ~: devContainerDockerfileFor devContainerConfig
                ~: devContainerJsonFor devContainerConfig projectName projectType
                ~: vsCodeSettingsFor devContainerConfig
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
          Node ".nix-bootstrap.toml - This holds nix-bootstrap's configuration to ensure upgrades are reliable." [],
          Node
            ".vscode"
            [ Node "settings.json - This configures the extensions provided by the VSCode DevContainer." []
            ],
          Node
            "nix"
            [ Node "pre-commit-hooks.nix - This configures which pre-commit hooks are used." [],
              Node "sources.json - This contains metadata about your nix dependencies." [],
              Node "sources.nix - This is the interface between nix and the dependencies listed in sources.json." []
            ],
          Node "README.md - This helpfully explains to you what each file (including itself) does!" [],
          Node "shell.nix - This configures what tools are available in your development environment and links in the pre-commit hooks." []
        ]
