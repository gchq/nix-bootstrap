-- |
-- Description : Bootstraps .vscode/extensions.json
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.VSCodeExtensions
  ( VSCodeExtensions,
    vsCodeExtensionsFileFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.ProjectType (ProjectType)
import Bootstrap.Data.VSCodeExtension (vsCodeExtensionsFor)
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
      ["recommendations" .= Aeson.Array (fromList . (toJSON <$>) $ vsCodeExtensionsFor projectType)]

-- | Constructs `VSCodeExtensions` for the given `ProjectType`
vsCodeExtensionsFileFor :: ProjectType -> Maybe VSCodeExtensions
vsCodeExtensionsFileFor = Just . VSCodeExtensions
