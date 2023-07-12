{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.IndexJs
  ( ElmIndexJs,
    elmIndexJsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
  )
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeNode),
    ElmOptions (ElmOptions),
    ProjectType (Elm),
  )
import Text.RawString.QQ (r)

data ElmIndexJs = ElmIndexJs

instance Bootstrappable ElmIndexJs where
  bootstrapName = const "src/index.js"
  bootstrapReason = const "Inserts your Elm code into index.html"
  bootstrapContent ElmIndexJs =
    pure $
      Right
        [r|import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("root"),
});
|]

elmIndexJsFor :: ProjectType -> Maybe ElmIndexJs
elmIndexJsFor = \case
  Elm (ElmOptions (ElmModeNode _) _) -> Just ElmIndexJs
  _ -> Nothing
