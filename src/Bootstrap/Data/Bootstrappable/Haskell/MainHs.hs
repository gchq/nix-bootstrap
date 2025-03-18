{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.MainHs
  ( MainHs,
    mainHsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    HaskellImport (HaskellImport),
    bootstrapContentHaskell,
    haskellModule,
    haskellModuleDecs,
    haskellModuleImports,
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
    Exp (VarE),
    ModName (ModName),
    Type (AppT, ConT),
    mkName,
  )

data MainHs = MainHs

instance Bootstrappable MainHs where
  bootstrapName = const "app/Main.hs"
  bootstrapReason = const "The entrypoint of your haskell executable"
  bootstrapContent MainHs = do
    let main = mkName "main"
        io = mkName "IO"
        lib = mkName "lib"
    unitType <- [t|()|]
    pure . pure . bootstrapContentHaskell $
      haskellModule (ModName "Main") (one "main")
        & haskellModuleImports
        ?~ one (HaskellImport (ModName "Lib") [lib])
          & haskellModuleDecs
        ?~ SigD main (AppT (ConT io) unitType)
          :| [FunD main [Clause [] (NormalB $ VarE lib) []]]

mainHsFor :: ProjectType -> Maybe MainHs
mainHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Just MainHs
  _ -> Nothing
