-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.VSCodeSettings (VSCodeSettings, vsCodeSettingsFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentPrettyJson,
  )
import Bootstrap.Data.DevContainer
  ( DevContainerConfig (DevContainerConfig),
  )
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson

data VSCodeSettings = VSCodeSettings

instance Bootstrappable VSCodeSettings where
  bootstrapName = const ".vscode/settings.json"
  bootstrapReason = const "This configures the settings for the extensions we recommend for VSCode."
  bootstrapContent = bootstrapContentPrettyJson []

instance ToJSON VSCodeSettings where
  toJSON =
    const $
      Aeson.object
        ["nixEnvSelector.nixFile" .= Aeson.String "${workspaceRoot}/shell.nix"]

vsCodeSettingsFor :: DevContainerConfig -> Maybe VSCodeSettings
vsCodeSettingsFor (DevContainerConfig True) = Just VSCodeSettings
vsCodeSettingsFor (DevContainerConfig False) = Nothing
