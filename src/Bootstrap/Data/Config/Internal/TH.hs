{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- |
-- Description : Functions used for generating code relating to Config
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Config.Internal.TH (makeConfigLenses) where

import Bootstrap.Data.Config.Internal (Config, _VersionedProjectType)
import Bootstrap.Data.ProjectType (ProjectType)
import Control.Lens (Lens')
import qualified Data.Char as C
import Data.Foldable (foldrM)
import qualified Data.Text as T
import Language.Haskell.TH
  ( Body (NormalB),
    Dec (SigD, ValD),
    DecsQ,
    Exp (InfixE, VarE),
    Name,
    Pat (VarP),
    Quote (newName),
    Type (AppT, ConT),
    lookupValueName,
    nameBase,
  )
import Language.Haskell.TH.Datatype as D
  ( ConstructorInfo (constructorFields, constructorVariant),
    ConstructorVariant (RecordConstructor),
    reifyConstructor,
  )
import qualified Relude.Unsafe as Unsafe

-- | Makes lenses for the given data constructor without
-- the config version number in them
makeConfigLenses :: Name -> DecsQ
makeConfigLenses name = do
  constructorInfo <- D.reifyConstructor name
  let D.RecordConstructor fieldNames = D.constructorVariant constructorInfo
      zipped = zip fieldNames (D.constructorFields constructorInfo)
  foldrM makeConfigLens [] zipped
  where
    makeConfigLens :: (Name, Language.Haskell.TH.Type) -> [Dec] -> DecsQ
    makeConfigLens (constructorName, constructorType) acc = do
      let baseFieldName = toText $ nameBase constructorName
          digits = T.takeWhile C.isDigit $ T.dropWhile (not . C.isDigit) baseFieldName
      fieldPrefix <-
        if T.null digits
          then fail "Field constructor should have config version digit in it"
          else pure $ "_configV" <> digits
      lensNameBody <-
        maybe
          (fail $ "Expected constructor to have prefix " <> toString fieldPrefix)
          pure
          (T.stripPrefix fieldPrefix baseFieldName)
      let lensNameStr = toString . ("config" <>) . (\(c, s) -> T.toUpper (one c) <> s) . Unsafe.fromJust $ T.uncons lensNameBody
      lensName <- newName lensNameStr
      versionedLensName <- Unsafe.fromJust <$> lookupValueName (drop 1 (toString fieldPrefix) <> toString lensNameBody)
      projectTypeType <- [t|ProjectType|]
      _VersionedProjectType' <- Just <$> [|_VersionedProjectType|]
      InfixE wrapped compose _ <- [e|_Current . x|]
      let isProjectTypeField = nameBase lensName == "configProjectType"
          versionedLens = Just $ VarE versionedLensName
          constructorType' = if isProjectTypeField then projectTypeType else constructorType
          body =
            NormalB . InfixE wrapped compose $
              if isProjectTypeField
                then Just (InfixE versionedLens compose _VersionedProjectType')
                else versionedLens
          sig = SigD lensName (AppT (AppT (ConT ''Lens') (ConT ''Config)) constructorType')
          impl = ValD (VarP lensName) body []
      pure $ acc <> [sig, impl]
