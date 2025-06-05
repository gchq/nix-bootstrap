{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.ServerHsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Haskell.ServerHs (serverHsFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeServer),
    ProjectType (Haskell),
    SetUpHaskellBuild (SetUpHaskellBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Server.hs rendering" do
  it "renders correctly" do
    bootstrapContent (serverHsFor . Haskell $ HaskellOptions (GHCVersion 9 0 2) (HaskellProjectTypeServer $ SetUpHaskellBuild True))
      >>= ( `shouldBe`
              Right
                [r|{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant (Application, Get, Handler, JSON, NoContent (NoContent), PutNoContent, ReqBody, Server, serve, type (:<|>) ((:<|>)), type (:>))

type API = "user" :> (ListUsersEndpoint :<|> PutUserEndpoint)
type ListUsersEndpoint = Get '[JSON] [User]
type PutUserEndpoint = ReqBody '[JSON] User :> PutNoContent
data User = User {name :: String, age :: Int} deriving stock Show

deriveJSON defaultOptions ''User

exampleUsers :: [User]
exampleUsers = [User "A. Example" 46, User "A. N. Other" 51]
app :: Application
app = serve (Proxy @API) server
server :: Server API
server = listUsersHandler :<|> putUserHandler
listUsersHandler :: Handler [User]
listUsersHandler = pure exampleUsers
putUserHandler :: User -> Handler NoContent
putUserHandler user = do {putTextLn ("I would insert the following user into the DB if this were real: " <> show user);
                          pure NoContent}
|]
          )
