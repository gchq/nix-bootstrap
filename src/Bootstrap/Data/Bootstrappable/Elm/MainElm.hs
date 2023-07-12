{-# LANGUAGE QuasiQuotes #-}

module Bootstrap.Data.Bootstrappable.Elm.MainElm (MainElm, mainElmFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
  )
import Bootstrap.Data.ProjectType (ProjectType (Elm))
import Text.RawString.QQ (r)

data MainElm = MainElm

instance Bootstrappable MainElm where
  bootstrapName = const "src/Main.elm"
  bootstrapReason = const "The entrypoint for your Elm code"
  bootstrapContent =
    const . pure $
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

mainElmFor :: ProjectType -> Maybe MainElm
mainElmFor = \case
  Elm _ -> Just MainElm
  _ -> Nothing
