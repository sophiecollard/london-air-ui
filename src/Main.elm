module Main exposing (..)

import Browser
import Header exposing (HeaderModel, HeaderMsg(..), initHeader, updateHeader, viewHeader)
import Html exposing (Html, div)
import Map exposing (..)



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
    { header : HeaderModel
    , map : MapModel
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        header =
            initHeader

        ( map, mapCmd ) =
            initMap header.species
    in
    ( Model header map, Cmd.map MapMsg mapCmd )



-- Msg & update


type Msg
    = HeaderMsg HeaderMsg
    | MapMsg MapMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeaderMsg headerMsg ->
            case headerMsg of
                EmitMapMsg mapMsg ->
                    handleMapMsg mapMsg model

                _ ->
                    handleHeaderMsg headerMsg model

        MapMsg mapMsg ->
            handleMapMsg mapMsg model


handleHeaderMsg : HeaderMsg -> Model -> ( Model, Cmd Msg )
handleHeaderMsg headerMsg model =
    let
        ( header, headerCmd ) =
            updateHeader headerMsg model.header
    in
    ( { model | header = header }
    , Cmd.map HeaderMsg headerCmd
    )


handleMapMsg : MapMsg -> Model -> ( Model, Cmd Msg )
handleMapMsg mapMsg model =
    let
        ( map, mapCmd ) =
            updateMap mapMsg model.map
    in
    ( { model | map = map }, Cmd.map MapMsg mapCmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model.header |> Html.map HeaderMsg
        , viewMap
        ]
