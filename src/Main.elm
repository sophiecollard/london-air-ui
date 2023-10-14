module Main exposing (..)

import Browser
import Html exposing (Html, a, div, h1, i, li, p, span, text, ul)
import Html.Attributes exposing (class, href, id, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, andThen, field, oneOf)



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
    { species : SpeciesCode
    , data : Data
    }


type Data
    = Loading
    | Loaded DailyAirQualityData
    | Error String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NO2 Loading, getDailyAirQualityData )


type alias DailyAirQualityData =
    { groupName : String
    , localAuthority : List LocalAuthority
    }


dailyAirQualityDataDecoder : Decoder DailyAirQualityData
dailyAirQualityDataDecoder =
    let
        innerDecoder =
            Json.Decode.map2 DailyAirQualityData
                (field "@GroupName" Json.Decode.string)
                (field "LocalAuthority" (Json.Decode.list localAuthorityDecoder))
    in
    Json.Decode.map identity
        (field "DailyAirQualityIndex" innerDecoder)


type alias LocalAuthority =
    { code : String
    , name : String
    , lat : Float
    , lng : Float
    , site : List Site
    }


localAuthorityDecoder : Decoder LocalAuthority
localAuthorityDecoder =
    Json.Decode.map5 LocalAuthority
        (field "@LocalAuthorityCode" Json.Decode.string)
        (field "@LocalAuthorityName" Json.Decode.string)
        (field "@LaCentreLatitude" Json.Decode.string |> andThen floatAsStringDecoder)
        (field "@LaCentreLongitude" Json.Decode.string |> andThen floatAsStringDecoder)
        (oneOf
            [ field "Site" (Json.Decode.list siteDecoder)
            , Json.Decode.succeed []
            ]
        )


type alias Site =
    { code : String
    , name : String
    , lat : Float
    , lng : Float
    , species : List Species
    }


siteDecoder : Decoder Site
siteDecoder =
    Json.Decode.map5 Site
        (field "@SiteCode" Json.Decode.string)
        (field "@SiteName" Json.Decode.string)
        (field "@Latitude" (Json.Decode.string |> andThen floatAsStringDecoder))
        (field "@Longitude" (Json.Decode.string |> andThen floatAsStringDecoder))
        (oneOf
            [ field "Species" (Json.Decode.list speciesDecoder)
            , Json.Decode.succeed []
            ]
        )


type alias Species =
    { code : SpeciesCode
    , airQualityIndex : AirQualityIndex
    , airQualityBand : String
    , source : String
    }


speciesDecoder : Decoder Species
speciesDecoder =
    Json.Decode.map4 Species
        (field "@SpeciesCode" speciesCodeDecoder)
        (field "@AirQualityIndex" airQualityIndexDecoder)
        (field "@AirQualityBand" Json.Decode.string)
        (field "@IndexSource" Json.Decode.string)


type AirQualityIndex
    = Low
    | Moderate
    | High
    | VeryHigh


airQualityIndexDecoder : Decoder AirQualityIndex
airQualityIndexDecoder =
    let
        fromString str =
            case str of
                "Low" ->
                    Json.Decode.succeed Low

                "Moderate" ->
                    Json.Decode.succeed Moderate

                "High" ->
                    Json.Decode.succeed High

                "Very High" ->
                    Json.Decode.succeed VeryHigh

                other ->
                    Json.Decode.fail ("Failed to decode air quality index from " ++ other)
    in
    Json.Decode.string |> andThen fromString


type SpeciesCode
    = CO
    | NO2
    | O3
    | PM10
    | PM25
    | SO2


allSpeciesCode : List SpeciesCode
allSpeciesCode =
    [ CO, NO2, O3, PM10, PM25, SO2 ]


speciesCodeToString : SpeciesCode -> String
speciesCodeToString code =
    case code of
        CO ->
            "Carbon Monoxide (CO)"

        NO2 ->
            "Nitrogen Dioxide (NO2)"

        O3 ->
            "Ozone (O3)"

        PM10 ->
            "PM10 Particulate"

        PM25 ->
            "PM2.5 Particulate"

        SO2 ->
            "Sulphur Dioxide (SO2)"


speciesCodeDecoder : Decoder SpeciesCode
speciesCodeDecoder =
    let
        fromString str =
            case str of
                "CO" ->
                    Json.Decode.succeed CO

                "NO2" ->
                    Json.Decode.succeed NO2

                "O3" ->
                    Json.Decode.succeed O3

                "PM10" ->
                    Json.Decode.succeed PM10

                "PM25" ->
                    Json.Decode.succeed PM25

                "SO2" ->
                    Json.Decode.succeed SO2

                other ->
                    Json.Decode.fail ("Failed to decode species code from " ++ other)
    in
    Json.Decode.string |> andThen fromString


floatAsStringDecoder : String -> Decoder Float
floatAsStringDecoder str =
    case String.toFloat str of
        Just float ->
            Json.Decode.succeed float

        Nothing ->
            Json.Decode.fail ("Failed to decode float from " ++ str)



-- Msg & update


type Msg
    = SelectSpecies SpeciesCode
    | GetDailyAirQualityData
    | GotDailyAirQualityData (Result Http.Error DailyAirQualityData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectSpecies speciesCode ->
            ( { model | species = speciesCode }, Cmd.none )

        GetDailyAirQualityData ->
            ( model, getDailyAirQualityData )

        GotDailyAirQualityData result ->
            case result of
                Ok dailyAirQualityData ->
                    ( { model | data = Loaded dailyAirQualityData }
                    , Cmd.none
                    )

                Err (Http.BadBody bodyError) ->
                    ( { model | data = Error ("Encountered bad body error: " ++ bodyError) }
                    , Cmd.none
                    )

                Err (Http.BadStatus status) ->
                    ( { model | data = Error ("Encountered bad status error: " ++ String.fromInt status) }
                    , Cmd.none
                    )

                Err (Http.BadUrl url) ->
                    ( { model | data = Error ("Encountered bad URL error: " ++ url) }
                    , Cmd.none
                    )

                Err Http.NetworkError ->
                    ( { model | data = Error "Encountered network error" }
                    , Cmd.none
                    )

                Err Http.Timeout ->
                    ( { model | data = Error "Encountered timeout error" }
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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    let
        contents =
            case model.data of
                Loading ->
                    p [] [ text "Loading data" ]

                Loaded dailyAirQualityData ->
                    div []
                        [ p [] [ text ("Got daily air quality index for " ++ dailyAirQualityData.groupName ++ ", with locales:") ]
                        , ul [] (List.map (\locale -> li [] [ text locale.name ]) dailyAirQualityData.localAuthority)
                        ]

                Error msg ->
                    p [] [ text msg ]
    in
    div []
        [ viewHeader model
        , div [ id "map", style "min-height" "90vh", style "z-index" "0" ] []
        , div [ class "container" ]
            [ div [ class "content" ]
                [ contents
                ]
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "hero is-small" ]
        [ div [ class "hero-body" ]
            [ div [ class "navbar" ]
                [ div [ class "navbar-brand" ]
                    [ div [ class "navbar-item" ]
                        [ h1 [ class "title is-3", style "text-shadow" "2px 2px #b2d3c2" ] [ text "London Air UI" ]
                        ]
                    ]
                , div [ class "navbar-menu" ]
                    [ div [ class "navbar-start" ]
                        [ div [ class "navbar-item has-dropdown is-hoverable" ]
                            [ div [ class "navbar-link" ]
                                [ text (speciesCodeToString model.species) ]
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
    if model.species == code then
        div [ class "navbar-item has-text-weight-semibold" ] [ text (speciesCodeToString code) ]

    else
        a [ class "navbar-item", onClick (SelectSpecies code) ] [ text (speciesCodeToString code) ]
