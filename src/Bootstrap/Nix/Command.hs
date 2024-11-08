{-# LANGUAGE ScopedTypeVariables #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Command
  ( NixCommandVariant (..),
    NixCommand (NixCommand),
    writeNixCommand,
  )
where

data NixCommandVariant = NCVBuild

newtype NixCommand = NixCommand
  { ncVariant :: NixCommandVariant
  }

-- | Prints a nix command in the form it would be used in a shell.
--
-- >>> writeNixCommand (NixCommand NCVBuild)
-- nix build
writeNixCommand :: forall s. (IsString s, Semigroup s) => NixCommand -> s
writeNixCommand NixCommand {..} = "nix " <> commandPart
  where
    commandPart :: s
    commandPart = case ncVariant of
      NCVBuild -> "build"
