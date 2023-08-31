-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.PreludeHs
  ( PreludeHs,
    preludeHsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    HaskellImport (HaskellImportAll),
    bootstrapContentHaskell,
    haskellModule,
    haskellModuleImports,
    haskellModulePragmas,
  )
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    ProjectType (Haskell),
  )
import Control.Lens ((?~))
import Language.Haskell.TH.Syntax (ModName (ModName))

data PreludeHs = PreludeHs

instance Bootstrappable PreludeHs where
  bootstrapName = const "src/Prelude.hs"
  bootstrapReason = const "The haskell prelude - what this exports is implicitly imported into every other module"
  bootstrapContent PreludeHs =
    pure . pure . bootstrapContentHaskell $
      haskellModule (ModName "Prelude") (one "module Relude")
        & haskellModulePragmas ?~ one "{-# OPTIONS_GHC -Wno-missing-import-lists #-}"
        & haskellModuleImports ?~ one (HaskellImportAll (ModName "Relude"))

preludeHsFor :: ProjectType -> Maybe PreludeHs
preludeHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic -> Just PreludeHs
  _ -> Nothing
