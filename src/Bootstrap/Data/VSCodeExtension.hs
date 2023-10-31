{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Utilities for working with VSCode extensions
module Bootstrap.Data.VSCodeExtension (VSCodeExtension (..), vsCodeExtensionsFor) where

import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    InstallLombok (unInstallLombok),
    JavaOptions (JavaOptions),
    ProjectType
      ( Elm,
        Go,
        Haskell,
        Java,
        Minimal,
        Node,
        Python,
        Rust
      ),
  )
import Data.Aeson (ToJSON)

-- | Represents the ID of an individual extension
newtype VSCodeExtension = VSCodeExtension Text
  deriving newtype (ToJSON)

-- | The list of extensions we recommend for the given `ProjectType`
vsCodeExtensionsFor :: ProjectType -> [VSCodeExtension]
vsCodeExtensionsFor =
  (VSCodeExtension <$>) . (["arrterian.nix-env-selector", "jnoortheen.nix-ide"] <>) . \case
    Minimal -> []
    Elm _ -> ["Elmtooling.elm-ls-vscode"]
    (Haskell (HaskellOptions _ _)) -> ["haskell.haskell"]
    Node _ -> []
    Go _ -> ["golang.Go"]
    Java (JavaOptions _ installLombok _) ->
      ["vscjava.vscode-java-pack"]
        <> ["gabrielbb.vscode-lombok" | unInstallLombok installLombok]
    Python _ -> ["ms-python.python"]
    Rust -> ["rust-lang.rust-analyzer"]
