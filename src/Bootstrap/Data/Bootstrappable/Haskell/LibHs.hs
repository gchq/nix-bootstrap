{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.LibHs
  ( LibHs,
    libHsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentHaskell,
    haskellModule,
    haskellModuleDecs,
  )
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    ProjectType (Haskell),
  )
import Control.Lens ((?~))
import Language.Haskell.TH.Syntax
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    Exp (AppE, VarE),
    ModName (ModName),
    Type (AppT, ConT),
    mkName,
  )

data LibHs = LibHs

instance Bootstrappable LibHs where
  bootstrapName = const "src/Lib.hs"
  bootstrapReason = const "The entrypoint of your haskell library"
  bootstrapContent LibHs = do
    let lib = mkName "lib"
        errorFunc = mkName "error"
        io = mkName "IO"
    unitType <- [t|()|]
    errorLine <- AppE (VarE errorFunc) <$> [|"todo: write the body of the lib function in src/Lib.hs"|]
    pure . pure . bootstrapContentHaskell $
      haskellModule (ModName "Lib") (one "lib")
        & haskellModuleDecs
        ?~ SigD lib (AppT (ConT io) unitType)
          :| [FunD lib [Clause [] (NormalB errorLine) []]]

libHsFor :: ProjectType -> Maybe LibHs
libHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Just LibHs
  _ -> Nothing
