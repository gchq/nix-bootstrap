{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- |
-- Description : Bootstraps .vscode/extensions.json
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.VSCodeExtensions
  ( VSCodeExtensions,
    vsCodeExtensionsFileFor,
    VSCodeExtension (..),
    extensionsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.ProjectType
  ( InstallLombok (unInstallLombok),
    ProjectType (Go, Java, Minimal, Node, Python),
  )
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson

-- | Represents the .vscode/extensions.json that we bootstrap
newtype VSCodeExtensions = VSCodeExtensions ProjectType

instance Bootstrappable VSCodeExtensions where
  bootstrapName = const ".vscode/extensions.json"
  bootstrapReason = const "This configures the extensions we recommend for VSCode."
  bootstrapContent = bootstrapContentPrettyJson []

instance ToJSON VSCodeExtensions where
  toJSON (VSCodeExtensions projectType) =
    Aeson.object
      ["recommendations" .= Aeson.Array (fromList . (toJSON <$>) $ extensionsFor projectType)]

-- | Constructs `VSCodeExtensions` for the given `ProjectType`
vsCodeExtensionsFileFor :: ProjectType -> Maybe VSCodeExtensions
vsCodeExtensionsFileFor = Just . VSCodeExtensions

-- | Represents the ID of an individual extension
newtype VSCodeExtension = VSCodeExtension Text
  deriving newtype (ToJSON)

-- | The list of extensions we recommend for the given `ProjectType`
extensionsFor :: ProjectType -> [VSCodeExtension]
extensionsFor =
  (VSCodeExtension <$>) . (["arrterian.nix-env-selector", "jnoortheen.nix-ide"] <>) . \case
    Minimal -> []
    Node _ -> []
    Go _ -> ["golang.Go"]
    Java _ installLombok _ ->
      ["vscjava.vscode-java-pack"]
        <> ["gabrielbb.vscode-lombok" | unInstallLombok installLombok]
    Python _ -> ["ms-python.python"]
