{-# LANGUAGE ScopedTypeVariables #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Command
  ( NixCommandStyle (..),
    NixCommandVariant (..),
    NixCommand (NixCommand),
    writeNixCommand,
  )
where

data NixCommandStyle = NCSOld | NCSNew

data NixCommandVariant = NCVBuild

data NixCommand = NixCommand
  { ncStyle :: NixCommandStyle,
    ncVariant :: NixCommandVariant
  }

-- | Prints a nix command in the form it would be used in a shell.
--
-- >>> writeNixCommand (NixCommand NCSOld NCVBuild)
-- nix-build
--
-- >>> writeNixCommand (NixCommand NCSNew NCVBuild)
-- nix build
writeNixCommand :: forall s. (IsString s, Semigroup s) => NixCommand -> s
writeNixCommand NixCommand {..} = "nix" <> separator <> commandPart
  where
    separator :: s
    separator = case ncStyle of NCSOld -> "-"; NCSNew -> " "
    commandPart :: s
    commandPart = case ncVariant of
      NCVBuild -> "build"
