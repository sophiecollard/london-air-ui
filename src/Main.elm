module Main exposing (..)

import Browser
import Header exposing (HeaderModel, initHeader)
import Html exposing (Html, a, div, h1, i, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Encode
import Leaflet
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
    ( { header = initHeader, map = initMap }, getDailyAirQualityData )



-- Msg & update


type Msg
    = SelectSpecies SpeciesCode
    | GetDailyAirQualityData
    | GotDailyAirQualityData (Result Http.Error DailyAirQualityData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectSpecies species ->
            let
                header =
                    model.header

                newModel =
                    { model | header = { header | species = species } }
            in
            ( newModel, updateMarkers newModel )

        GetDailyAirQualityData ->
            ( model, getDailyAirQualityData )

        GotDailyAirQualityData result ->
            let
                map =
                    model.map
            in
            case result of
                Ok dailyAirQualityData ->
                    let
                        newModel =
                            { model | map = { map | data = Loaded dailyAirQualityData } }
                    in
                    ( newModel, updateMarkers newModel )

                Err (Http.BadBody bodyError) ->
                    ( { model | map = { map | data = Error ("Encountered bad body error: " ++ bodyError) } }
                    , Cmd.none
                    )

                Err (Http.BadStatus status) ->
                    ( { model | map = { map | data = Error ("Encountered bad status error: " ++ String.fromInt status) } }
                    , Cmd.none
                    )

                Err (Http.BadUrl url) ->
                    ( { model | map = { map | data = Error ("Encountered bad URL error: " ++ url) } }
                    , Cmd.none
                    )

                Err Http.NetworkError ->
                    ( { model | map = { map | data = Error "Encountered network error" } }
                    , Cmd.none
                    )

                Err Http.Timeout ->
                    ( { model | map = { map | data = Error "Encountered timeout error" } }
                    , Cmd.none
                    )


getDailyAirQualityData : Cmd Msg
getDailyAirQualityData =
    Http.get
        { url = dailyAirQualityDataUrl
        , expect = Http.expectJson GotDailyAirQualityData dailyAirQualityDataDecoder
        }


dailyAirQualityDataUrl : String
dailyAirQualityDataUrl =
    "https://api.erg.ic.ac.uk/AirQuality/Daily/MonitoringIndex/Latest/GroupName=London/Json"


updateMarkers : Model -> Cmd msg
updateMarkers model =
    let
        markers =
            case model.map.data of
                Loaded data ->
                    markersFromData data model.header.species

                _ ->
                    []
    in
    Json.Encode.list markerEncoder markers
        |> Leaflet.resetMarkers



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
                                (List.map (viewDropdownItem model) allSpeciesCode)
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


viewDropdownItem : Model -> SpeciesCode -> Html Msg
viewDropdownItem model code =
    if model.header.species == code then
        div [ class "navbar-item has-text-weight-semibold" ] [ text (speciesCodeToString code) ]

    else
        a [ class "navbar-item", onClick (SelectSpecies code) ] [ text (speciesCodeToString code) ]
