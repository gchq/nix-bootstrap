{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.ServerHs
  ( ServerHs (ServerHs),
    serverHsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    HaskellImport (HaskellImport),
    bootstrapContentHaskell,
    haskellModule,
    haskellModuleDecs,
    haskellModuleImports,
    haskellModulePragmas,
  )
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly, HaskellProjectTypeServer),
    ProjectType (Haskell),
  )
import Control.Lens ((?~))
import qualified Data.Text as T
import Language.Haskell.TH
  ( DerivStrategy (StockStrategy),
    appT,
    appTypeE,
    clause,
    conE,
    conT,
    dataD,
    derivClause,
    doE,
    funD,
    infixE,
    infixT,
    listT,
    litT,
    noBindS,
    normalB,
    recC,
    sigD,
    strTyLit,
    tySynD,
    valD,
    varE,
    varP,
  )
import Language.Haskell.TH.Syntax
  ( Bang (Bang),
    ModName (ModName),
    SourceStrictness (NoSourceStrictness),
    SourceUnpackedness (NoSourceUnpackedness),
    mkName,
  )

data ServerHs = ServerHs

instance Bootstrappable ServerHs where
  bootstrapName = const "src/Server.hs"
  bootstrapReason = const "The entrypoint of your haskell server"
  bootstrapContent ServerHs = do
    string <- conT $ mkName "String"
    int <- conT $ mkName "Int"
    let bang = Bang NoSourceUnpackedness NoSourceStrictness
        httpGet = mkName "Get"
        json = mkName "JSON"
        user = mkName "User"
        reqBody = mkName "ReqBody"
        happyBird = mkName ":>"
        noisyBird = mkName ":<|>"
        putNoContent = mkName "PutNoContent"
        name = mkName "name"
        age = mkName "age"
        listUsersEndpoint = mkName "ListUsersEndpoint"
        putUserEndpoint = mkName "PutUserEndpoint"
        api = mkName "API"
        exampleUsers = mkName "exampleUsers"
        app = mkName "app"
        application = mkName "Application"
        server = mkName "server"
        handler = mkName "Handler"
        noContent = mkName "NoContent"
        listUsersHandler = mkName "listUsersHandler"
        putUserHandler = mkName "putUserHandler"
        userVar = mkName "user"
        pureN = mkName "pure"
        sconcatN = mkName "<>"
        showN = mkName "show"
    jsonListType <- [t|'[$(conT json)]|]
    userDec <-
      dataD
        (pure [])
        user
        []
        Nothing
        [recC user [pure (name, bang, string), pure (age, bang, int)]]
        [derivClause (Just StockStrategy) [[t|$(conT $ mkName "Show")|]]]
    listUsersEndpointDec <-
      tySynD listUsersEndpoint [] $
        conT httpGet
          `appT` pure jsonListType
          `appT` (listT `appT` conT user)
    putUserEndpointDec <-
      tySynD putUserEndpoint [] $
        infixT
          (conT reqBody `appT` pure jsonListType `appT` conT user)
          happyBird
          (conT putNoContent)
    apiDec <- tySynD api [] $ infixT (litT $ strTyLit "user") happyBird (infixT (conT listUsersEndpoint) noisyBird (conT putUserEndpoint))
    exampleUsersSigDec <- sigD exampleUsers (listT `appT` conT user)
    exampleUsersDec <- valD (varP exampleUsers) (normalB [|[$(conE user) "A. Example" 46, $(conE user) "A. N. Other" 51]|]) []
    appSigDec <- sigD app $ conT application
    appDec <- valD (varP app) (normalB [|$(conE $ mkName "serve") ($(appTypeE (conE $ mkName "Proxy") (conT api))) $(conE server)|]) []
    serverSigDec <- sigD server [t|$(conT $ mkName "Server") $(conT api)|]
    serverDec <- valD (varP server) (normalB $ infixE (Just $ varE listUsersHandler) (varE noisyBird) (Just $ varE putUserHandler)) []
    listUsersHandlerSigDec <- sigD listUsersHandler [t|$(conT handler) [$(conT user)]|]
    listUsersHandlerDec <- valD (varP listUsersHandler) (normalB [|$(varE pureN) $(varE exampleUsers)|]) []
    putUserHandlerSigDec <- sigD putUserHandler [t|$(conT user) -> $(conT handler) $(conT noContent)|]
    putUserHandlerDec <-
      funD
        putUserHandler
        [ clause
            [varP userVar]
            ( normalB $
                doE
                  [ noBindS
                      [|
                        putTextLn
                          $( infixE
                               (pure [|"I would insert the following user into the DB if this were real: "|])
                               (varE sconcatN)
                               $ Just [|$(varE showN) $(varE userVar)|]
                           )
                        |],
                    noBindS [|$(varE pureN) $(varE noContent)|]
                  ]
            )
            []
        ]
    pure
      . pure
      . T.replace "exampleUsers ::" "\nderiveJSON defaultOptions ''User\n\nexampleUsers ::"
      . T.replace "('(:) JSON '[])" "'[JSON]"
      . bootstrapContentHaskell
      $ haskellModule (ModName "Server") (one "app")
        & haskellModulePragmas
        ?~ "{-# LANGUAGE DataKinds #-}"
          :| [ "{-# LANGUAGE TemplateHaskell #-}",
               "{-# LANGUAGE TypeApplications #-}",
               "{-# LANGUAGE TypeOperators #-}"
             ]
          & haskellModuleImports
        ?~ HaskellImport (ModName "Data.Aeson") ["defaultOptions"]
          :| [ HaskellImport (ModName "Data.Aeson.TH") ["deriveJSON"],
               HaskellImport
                 (ModName "Servant")
                 [ "Application",
                   "Get",
                   "Handler",
                   "JSON",
                   "NoContent (NoContent)",
                   "PutNoContent",
                   "ReqBody",
                   "Server",
                   "serve",
                   "type (:<|>) ((:<|>))",
                   "type (:>)"
                 ]
             ]
          & haskellModuleDecs
        ?~ apiDec
          :| [ listUsersEndpointDec,
               putUserEndpointDec,
               userDec,
               exampleUsersSigDec,
               exampleUsersDec,
               appSigDec,
               appDec,
               serverSigDec,
               serverDec,
               listUsersHandlerSigDec,
               listUsersHandlerDec,
               putUserHandlerSigDec,
               putUserHandlerDec
             ]

serverHsFor :: ProjectType -> Maybe ServerHs
serverHsFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Nothing
    HaskellProjectTypeServer _ -> Just ServerHs
  _ -> Nothing
