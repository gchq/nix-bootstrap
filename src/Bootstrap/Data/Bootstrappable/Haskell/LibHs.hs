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
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly, HaskellProjectTypeServer),
    ProjectType (Haskell),
  )
import Control.Lens ((?~))
import Language.Haskell.TH (conT, varE)
import Language.Haskell.TH.Syntax
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    ModName (ModName),
    mkName,
  )

data LibHs = LibHs

instance Bootstrappable LibHs where
  bootstrapName = const "src/Lib.hs"
  bootstrapReason = const "The entrypoint of your haskell library"
  bootstrapContent LibHs = do
    let lib = mkName "lib"
    ioUnit <- [t|$(conT $ mkName "IO") ()|]
    errorLine <- [|$(varE $ mkName "error") "todo: write the body of the lib function in src/Lib.hs"|]
    pure . pure . bootstrapContentHaskell $
      haskellModule (ModName "Lib") (one "lib")
        & haskellModuleDecs
        ?~ SigD lib ioUnit
          :| [FunD lib [Clause [] (NormalB errorLine) []]]

libHsFor :: ProjectType -> Maybe LibHs
libHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Just LibHs
    HaskellProjectTypeServer _ -> Nothing
  _ -> Nothing
