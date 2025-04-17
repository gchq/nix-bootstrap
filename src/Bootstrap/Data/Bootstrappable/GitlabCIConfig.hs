-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitlabCIConfig
  ( GitlabCIConfig,
    gitlabCIConfigFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
  )
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig
  ( NixPreCommitHookConfig (nixPreCommitHookConfigImpureHookCommand),
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (ElmOptions, elmOptionElmMode),
    JavaOptions (JavaOptions),
    NodePackageManager (NPM, PNPm, Yarn),
    ProjectType (Elm, Go, Java),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (SetUpJavaBuild),
  )

data GitlabCIConfig = GitlabCIConfig
  { gitlabCIConfigProjectType :: ProjectType,
    gitlabCIConfigPreCommitHooksConfig :: Maybe NixPreCommitHookConfig
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
        case nixPreCommitHookConfigImpureHookCommand <$> gitlabCIConfigPreCommitHooksConfig of
          Just c | c /= "echo ok" -> "check-dev-environment-and-run-impure-hooks:"
          _ -> "check-dev-environment:",
        "  stage: build",
        "  script:",
        commandInShell $
          maybe
            "echo ok"
            nixPreCommitHookConfigImpureHookCommand
            gitlabCIConfigPreCommitHooksConfig
      ]
        <> ( if isJust gitlabCIConfigPreCommitHooksConfig
               then
                 [ "",
                   "pre-commit-check:",
                   "  stage: build",
                   "  script:",
                   "    - nix flake check"
                 ]
               else []
           )
        <> case gitlabCIConfigProjectType of
          Elm opts -> elmSiteJob opts
          Go (SetUpGoBuild True) -> buildJob
          Java (JavaOptions _ _ (SetUpJavaBuild _) _) -> buildJob
          _ -> []
    where
      buildJob :: [Text]
      buildJob =
        [ "",
          "build:",
          "  stage: build",
          "  script:",
          "    - nix build"
        ]
      elmSiteJob :: ElmOptions -> [Text]
      elmSiteJob ElmOptions {elmOptionElmMode} =
        [ "",
          "build-site:",
          "  stage: build",
          "  script:"
        ]
          <> ( commandInShell <$> case elmOptionElmMode of
                 ElmModeBare -> ["elm make src/Main.elm"]
                 ElmModeNode packageManager ->
                   [ nodePackageManagerInstall packageManager,
                     runWithPackageManager packageManager <> "build"
                   ]
             )
      commandInShell :: Text -> Text
      commandInShell = ("    - nix develop -c " <>)
      nodePackageManagerInstall :: NodePackageManager -> Text
      nodePackageManagerInstall = \case
        NPM -> "npm ci"
        Yarn -> "yarn install --frozen-lockfile"
        PNPm -> "pnpm install --frozen-lockfile"
      runWithPackageManager :: NodePackageManager -> Text
      runWithPackageManager =
        (<> " run ") . \case
          NPM -> "npm"
          Yarn -> "yarn"
          PNPm -> "pnpm"

gitlabCIConfigFor ::
  ContinuousIntegrationConfig ->
  ProjectType ->
  Maybe NixPreCommitHookConfig ->
  Maybe GitlabCIConfig
gitlabCIConfigFor (ContinuousIntegrationConfig False) _ _ = Nothing
gitlabCIConfigFor (ContinuousIntegrationConfig True) t p =
  Just $ GitlabCIConfig t p
