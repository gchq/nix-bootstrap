-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.IndexHtml
  ( ElmIndexHtml,
    elmIndexHtmlFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (ElmMode (ElmModeNode), ElmOptions (ElmOptions), ProjectType (Elm))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.XHtml5 ((!))
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as HAttr

newtype ElmIndexHtml = ElmIndexHtml ProjectName

instance Bootstrappable ElmIndexHtml where
  bootstrapName = const "src/index.html"
  bootstrapReason = const "The top-level HTML file for your site"
  bootstrapContent (ElmIndexHtml projectName) =
    pure . Right . toText . renderHtml $ H.docTypeHtml do
      H.head do
        H.meta ! HAttr.charset "utf-8"
        H.title . H.toHtml $ unProjectName projectName
        H.script ! HAttr.type_ "module" ! HAttr.src "./index.js" $ mempty
      H.body $ H.div ! HAttr.id "root" $ mempty

elmIndexHtmlFor :: ProjectName -> ProjectType -> Maybe ElmIndexHtml
elmIndexHtmlFor projectName = \case
  Elm (ElmOptions (ElmModeNode _) _) -> Just $ ElmIndexHtml projectName
  _ -> Nothing
