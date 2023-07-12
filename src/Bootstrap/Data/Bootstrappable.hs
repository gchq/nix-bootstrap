-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable
  ( Bootstrappable (..),
    bootstrapContentNix,
    bootstrapContentPrettyJson,
    bootstrapContentYaml,
  )
where

import Bootstrap.Nix.Expr
  ( Identifier (unIdentifier),
    IsNixExpr (toNixExpr),
    isMostlyCorrectlyScoped,
    writeExprFormatted,
  )
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
  ( Config (confCompare),
    defConfig,
    encodePretty',
    keyOrder,
  )
import qualified Data.Yaml.Aeson as Yaml

-- | Things which can be written out as files during the bootstrapping
-- process.
class Bootstrappable a where
  -- | Wraps the value in a Just constructor. Should only use Nothing
  -- when there can be no meaningful bootstrapping performed from the
  -- value.
  bootstrapWithMaybe :: a -> Maybe a
  bootstrapWithMaybe = Just

  -- | The filename of the file when bootstrapped
  bootstrapName :: a -> FilePath

  -- | The human-readable reason we bootstrap this file
  bootstrapReason :: a -> Text

  -- | The contents of the file
  bootstrapContent :: MonadIO m => a -> m (Either String Text)

-- | The Maybe instance for Bootstrappable allows handling of files which
-- may have been bootstrapped, but now shouldn't be due to some condition.
--
-- If given a `Just` value, it behaves the same way as the underlying instance.
--
-- If given a Nothing value, it **must not** be bootstrapped; any attempts to do
-- so will call `error`. This can be safely avoided by checking for a `Nothing`
-- value using `bootstrapWithMaybe`.
instance Bootstrappable a => Bootstrappable (Maybe a) where
  bootstrapWithMaybe (Just a) = Just (Just a)
  bootstrapWithMaybe Nothing = Nothing
  bootstrapName = maybe (error "Called bootstrapName on Nothing value") bootstrapName
  bootstrapReason = maybe (error "Called bootstrapReason on Nothing value") bootstrapReason
  bootstrapContent (Just a) = bootstrapContent a
  bootstrapContent Nothing = error "Called bootstrapContent on Nothing value"

-- | A helper function when the generated file should be nix code
bootstrapContentNix :: (IsNixExpr a, MonadIO m) => a -> m (Either String Text)
bootstrapContentNix a = do
  let expr = toNixExpr a
   in case isMostlyCorrectlyScoped expr of
        Right () -> do
          first
            ( ("Could not format nix expression: " <>)
                . displayException
            )
            <$> writeExprFormatted expr
        Left (i1 :| iRest) ->
          pure . Left $
            "Nix expression is incorrectly scoped; it references the out of scope "
              <> ( if null iRest
                     then "identifier " <> toString (unIdentifier i1)
                     else
                       "identifiers "
                         <> toString
                           ( foldr (\i acc -> unIdentifier i <> ", " <> acc) ("and " <> unIdentifier i1) iRest
                           )
                 )
              <> ". This is a bug in nix-bootstrap; please "
              <> "contact the nix-bootstrap team to report it."

-- | A helper function when the generated file should be JSON
--
-- Keys in the first argument will appear first in their given order. Other keys
-- will be ordered alphabetically.
bootstrapContentPrettyJson :: (ToJSON a, Monad m) => [Text] -> a -> m (Either String Text)
bootstrapContentPrettyJson keysToPrioritise =
  pure
    . Right
    . (<> "\n")
    . decodeUtf8With lenientDecode
    . toStrict
    . encodePretty'
      defConfig {confCompare = keyOrder keysToPrioritise <> compare}

-- | A helper function when the generated file should be YAML
bootstrapContentYaml :: ToJSON a => a -> Text
bootstrapContentYaml = decodeUtf8With lenientDecode . Yaml.encode
