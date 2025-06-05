{-# LANGUAGE QuasiQuotes #-}
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
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly, HaskellProjectTypeServer),
    ProjectType (Haskell),
  )
import Control.Lens ((?~))
import Language.Haskell.TH (appE, conT, varE)
import Language.Haskell.TH.Syntax
  ( Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    Exp (DoE, VarE),
    ModName (ModName),
    Stmt (NoBindS),
    mkName,
  )

data MainHs
  = -- | Simply calls Lib.lib
    MainHsLib
  | -- | Uses Server.app (a WAI Application) to run a warp server on port 8080
    MainHsWarp

instance Bootstrappable MainHs where
  bootstrapName = const "app/Main.hs"
  bootstrapReason = const "The entrypoint of your haskell executable"
  bootstrapContent mainHs = do
    let app = mkName "app"
        lib = mkName "lib"
        main = mkName "main"
        run = mkName "run"
    ioUnit <- [t|$(conT $ mkName "IO") ()|]
    logServing <- [|putTextLn "Serving on 8080..."|]
    runServer <- varE run `appE` [|8080|] `appE` varE app
    pure . pure . bootstrapContentHaskell $
      haskellModule (ModName "Main") (one "main")
        & haskellModuleImports
        ?~ ( case mainHs of
               MainHsLib -> one (HaskellImport (ModName "Lib") ["lib"])
               MainHsWarp ->
                 HaskellImport (ModName "Network.Wai.Handler.Warp") ["run"]
                   :| [HaskellImport (ModName "Server") ["app"]]
           )
          & haskellModuleDecs
        ?~ SigD main ioUnit
          :| [ FunD
                 main
                 [ Clause
                     []
                     ( NormalB $ case mainHs of
                         MainHsLib -> VarE lib
                         MainHsWarp -> DoE Nothing [NoBindS logServing, NoBindS runServer]
                     )
                     []
                 ]
             ]

mainHsFor :: ProjectType -> Maybe MainHs
mainHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Just MainHsLib
    HaskellProjectTypeServer _ -> Just MainHsWarp
  _ -> Nothing
