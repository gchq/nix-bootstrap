{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ProjectName
  ( ProjectName (unProjectName),
    mkProjectName,
    replaceSpacesWithDashes,
    tomlBiMap,
  )
where

import qualified Data.Char as C
import qualified Data.Text as T
import Dhall (FromDhall, ToDhall)
import qualified Toml

-- | The name of a project. It can be comprised of characters matching [-_a-zA-Z0-9 ]
newtype ProjectName = ProjectName {unProjectName :: Text}
  deriving stock (Eq, Show)
  deriving newtype (FromDhall, ToDhall)

-- | A smart constructor which gives back Nothing if the project name
-- contains invalid characters or begins with a non-alphabetical character.
mkProjectName :: Text -> Maybe ProjectName
mkProjectName name =
  if T.all C.isAlpha (T.take 1 name) && T.all isValidChar name
    then Just $ ProjectName name
    else Nothing
  where
    isValidChar :: Char -> Bool
    isValidChar = \case
      '_' -> True
      '-' -> True
      ' ' -> True
      c -> C.isAlphaNum c

replaceSpacesWithDashes :: ProjectName -> ProjectName
replaceSpacesWithDashes = ProjectName . T.replace " " "-" . unProjectName

tomlBiMap :: Toml.TomlBiMap ProjectName Toml.AnyValue
tomlBiMap = Toml.BiMap {forward, backward}
  where
    forward :: ProjectName -> Either Toml.TomlBiMapError Toml.AnyValue
    forward projectName = Right . Toml.AnyValue . Toml.Text $ unProjectName projectName
    backward :: Toml.AnyValue -> Either Toml.TomlBiMapError ProjectName
    backward (Toml.AnyValue v) = case v of
      Toml.Text t ->
        maybeToRight
          (Toml.ArbitraryError "Invalid project name")
          (mkProjectName t)
      _ -> first Toml.WrongValue $ Toml.mkMatchError Toml.TText v
