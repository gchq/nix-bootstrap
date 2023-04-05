-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitlabCIConfig
  ( GitlabCIConfig,
    gitlabCIConfigFor,
  )
where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (unPreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectType
  ( JavaOptions (JavaOptions),
    ProjectType (Go, Java),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (SetUpJavaBuild),
  )

data GitlabCIConfig = GitlabCIConfig
  { gitlabCIConfigUseFlakes :: Bool,
    gitlabCIConfigProjectType :: ProjectType,
    gitlabCIConfigPreCommitHooksConfig :: PreCommitHooksConfig
  }

instance Bootstrappable GitlabCIConfig where
  bootstrapName = const ".gitlab-ci.yml"
  bootstrapReason = const "This file sets up GitLab CI."
  bootstrapContent GitlabCIConfig {..} = do
    pure . Right . unlines $
      [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
        "",
        "default:",
        "  before_script:",
        "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
        "    - nix-channel --update",
        "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
        "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
        "",
        "check-dev-environment:",
        "  stage: build",
        "  script: \""
          <> ( if gitlabCIConfigUseFlakes
                 then "nix develop -c echo ok"
                 else "nix-shell --run 'echo ok'"
             )
          <> "\""
      ]
        <> ( if unPreCommitHooksConfig gitlabCIConfigPreCommitHooksConfig
               then
                 [ "",
                   "pre-commit-check:",
                   "  stage: build",
                   "  script: \""
                     <> ( if gitlabCIConfigUseFlakes
                            then "nix build '.#runChecks'"
                            else "nix-build nix/pre-commit-hooks.nix -A hooks --arg pre-commit-hooks-lib 'import (import nix/sources.nix {}).pre-commit-hooks'"
                        )
                     <> "\""
                 ]
               else []
           )
        <> case gitlabCIConfigProjectType of
          Go (SetUpGoBuild True) -> buildJob
          Java (JavaOptions _ _ (SetUpJavaBuild _)) -> buildJob
          _ -> []
    where
      buildJob :: [Text]
      buildJob =
        [ "",
          "build:",
          "  stage: build",
          "  script: \""
            <> ( if gitlabCIConfigUseFlakes
                   then "nix build"
                   else "nix-build"
               )
            <> "\""
        ]

gitlabCIConfigFor ::
  ContinuousIntegrationConfig ->
  RunConfig ->
  ProjectType ->
  PreCommitHooksConfig ->
  Maybe GitlabCIConfig
gitlabCIConfigFor (ContinuousIntegrationConfig False) _ _ _ = Nothing
gitlabCIConfigFor (ContinuousIntegrationConfig True) RunConfig {rcUseFlakes} t p =
  Just $ GitlabCIConfig rcUseFlakes t p
