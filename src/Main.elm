module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)



-- Main


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model & init


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- Msg & update


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view _ =
    div [] [ h1 [ class "title is-1" ] [ text "Hello, World!" ] ]
