module Main exposing (..)

import Browser
import Header exposing (HeaderModel, HeaderMsg(..), initHeader, updateHeader)
import Html exposing (Html, a, div, h1, i, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Html.Events exposing (onClick)
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
        [ viewHeader model
        , div [ id "map", style "min-height" "90vh", style "z-index" "0" ] []
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "hero is-small" ]
        [ div [ class "hero-body" ]
            [ div [ class "navbar" ]
                [ div [ class "navbar-brand" ]
                    [ div [ class "navbar-item" ]
                        [ h1 [ class "title is-3", style "text-shadow" "2px 2px #b2d3c2" ] [ text "London Air" ]
                        ]
                    ]
                , div [ class "navbar-menu" ]
                    [ div [ class "navbar-start" ]
                        [ div [ class "navbar-item has-dropdown is-hoverable" ]
                            [ div [ class "navbar-link" ]
                                [ text (speciesCodeToString model.header.species) ]
                            , div [ class "navbar-dropdown" ]
                                (allSpeciesCode |> List.map (viewDropdownItem model) |> List.map (Html.map HeaderMsg))
                            ]
                        ]
                    , div [ class "navbar-end" ]
                        [ div [ class "navbar-item" ]
                            [ a
                                [ class "button has-text-weight-semibold"
                                , href "https://www.londonair.org.uk/LondonAir/API/"
                                , target "_blank"
                                ]
                                [ span [ class "icon" ] [ i [ class "fa-solid fa-server" ] [] ]
                                , span [] [ text "London Air API" ]
                                ]
                            ]
                        , div [ class "navbar-item" ]
                            [ a
                                [ class "button is-success has-text-weight-semibold"
                                , href "https://github.com/sophiecollard/london-air-ui"
                                , target "_blank"
                                ]
                                [ span [ class "icon" ] [ i [ class "fab fa-github" ] [] ]
                                , span [] [ text "View on GitHub" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewDropdownItem : Model -> SpeciesCode -> Html HeaderMsg
viewDropdownItem model code =
    if model.header.species == code then
        div [ class "navbar-item has-text-weight-semibold" ] [ text (speciesCodeToString code) ]

    else
        a [ class "navbar-item", onClick (SelectSpecies code) ] [ text (speciesCodeToString code) ]
