{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.MainElmSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.MainElm (mainElmFor)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare),
    ElmOptions (ElmOptions),
    ProjectType (Elm),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Main.elm rendering" do
  it "renders the Main.elm correctly" do
    bootstrapContent (mainElmFor . Elm $ ElmOptions ElmModeBare True)
      >>= ( `shouldBe`
              Right
                [r|module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    ()


init : Model
init =
    ()


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view () =
    button [ onClick NoOp ] [ text "Hello, world!" ]
|]
          )
