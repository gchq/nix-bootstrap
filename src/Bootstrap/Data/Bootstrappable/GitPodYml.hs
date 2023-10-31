-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml, gitPodYmlFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
    bootstrapContentYaml,
  )
import Bootstrap.Data.ProjectType (ProjectType)
import Bootstrap.Data.VSCodeExtension (vsCodeExtensionsFor)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (emptyArray)

newtype GitPodYml = GitPodYml ProjectType

instance Bootstrappable GitPodYml where
  bootstrapName = const ".gitpod.yml"
  bootstrapReason = const "This overrides GitPod's automated tasks; they are not needed."
  bootstrapContent = pure . Right . bootstrapContentYaml

instance ToJSON GitPodYml where
  toJSON (GitPodYml projectType) =
    Aeson.object
      [ "tasks" .= emptyArray,
        "vscode" .= Aeson.object ["extensions" .= toJSON (vsCodeExtensionsFor projectType)]
      ]

gitPodYmlFor :: ProjectType -> Maybe GitPodYml
gitPodYmlFor = Just . GitPodYml
