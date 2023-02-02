-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml (GitPodYml)) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason), bootstrapContentYaml)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (emptyArray)

data GitPodYml = GitPodYml

instance Bootstrappable GitPodYml where
  bootstrapName = const ".gitpod.yml"
  bootstrapReason = const "This overrides GitPod's automated tasks; they are not needed."
  bootstrapContent = pure . Right . bootstrapContentYaml

instance ToJSON GitPodYml where
  toJSON = const $ Aeson.object ["tasks" .= emptyArray]
