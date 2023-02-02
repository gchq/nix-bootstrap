-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc)) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig, unPreCommitHooksConfig)

data Envrc = Envrc {preCommitHooksConfig :: PreCommitHooksConfig, useFlakes :: Bool}

instance Bootstrappable Envrc where
  bootstrapName = const ".envrc"
  bootstrapReason Envrc {preCommitHooksConfig} =
    "This tells direnv to load the nix shell"
      <> if unPreCommitHooksConfig preCommitHooksConfig
        then " and set up the pre-commit hooks."
        else "."
  bootstrapContent Envrc {preCommitHooksConfig, useFlakes} =
    pure . Right . unlines $
      (if useFlakes then flakeLines else [])
        <> ( (if useFlakes then "use flake" else "use_nix") :
               ["eval \"$shellHook\"" | unPreCommitHooksConfig preCommitHooksConfig]
           )
    where
      flakeLines :: [Text]
      flakeLines =
        [ "direnv version 2.23.0 || exit 1",
          "if [ $(nix-env --version | grep -oE '[0-9]+\\.[0-9]+' | head -n1 | sed 's/\\./000/') -lt 20004 ]; then",
          "  echo 'This project is set up to work with Nix Flakes, which your version of nix doesn'\"'\"'t support.'",
          "  echo 'Please upgrade your nix version to at least 2.4 to continue.'",
          "  exit 1",
          "fi",
          "if ! nix show-config --extra-experimental-features nix-command | grep experimental-features | grep flakes 1>/dev/null 2>&1; then",
          "  printf '\\033[31m'",
          "  echo 'This project is set up to work with Nix Flakes, which you don'\"'\"'t currently have enabled.'",
          "  echo 'Please enable flakes by following the instructions at https://nixos.wiki/wiki/flakes#Installing_flakes'",
          "  printf '\\033[0m'",
          "  exit 1",
          "fi",
          "if ! nix show-config 1>/dev/null 2>&1; then",
          "  printf '\\033[31m'",
          "  echo 'This project is set up to work with Nix Flakes, which you don'\"'\"'t currently have enabled.'",
          "  echo 'Specifically, the \"nix-command\" option is missing from your nix experimental-features configuration.'",
          "  echo 'Please enable flakes by following the instructions at https://nixos.wiki/wiki/flakes#Installing_flakes'",
          "  printf '\\033[0m'",
          "  exit 1",
          "fi"
        ]
