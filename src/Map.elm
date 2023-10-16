module Map exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (id, style)
import Http
import Json.Decode exposing (Decoder, andThen, field, oneOf)
import Json.Encode
import Leaflet
import Utils.List



-- Model & init


type alias MapModel =
    { data : Data
    }


initMap : SpeciesCode -> ( MapModel, Cmd MapMsg )
initMap speciesCode =
    ( { data = Loading }
    , getDailyAirQualityData speciesCode
    )


type Data
    = Loading
    | Loaded DailyAirQualityData
    | Error String


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
    , airQualityIndex : String
    , airQualityBand : AirQualityBand
    , source : String
    }


speciesDecoder : Decoder Species
speciesDecoder =
    Json.Decode.map4 Species
        (field "@SpeciesCode" speciesCodeDecoder)
        (field "@AirQualityIndex" Json.Decode.string)
        (field "@AirQualityBand" airQualityBandDecoder)
        (field "@IndexSource" Json.Decode.string)


type AirQualityBand
    = Low
    | Moderate
    | High
    | VeryHigh


airQualityBandToString : AirQualityBand -> String
airQualityBandToString band =
    case band of
        Low ->
            "Low"

        Moderate ->
            "Moderate"

        High ->
            "High"

        VeryHigh ->
            "Very High"


airQualityBandDecoder : Decoder AirQualityBand
airQualityBandDecoder =
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
            "PM10 Particules"

        PM25 ->
            "PM2.5 Particules"

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


type alias Marker =
    { lat : Float
    , lng : Float
    , color : MarkerColor
    , tooltip : String
    , popupContents : String
    }


markersFromData : DailyAirQualityData -> SpeciesCode -> List Marker
markersFromData data code =
    data.localAuthority
        |> Utils.List.flatMap .site
        |> Utils.List.flatMap (\site -> List.map (\s -> ( site, s )) site.species)
        |> List.filter (\( _, species ) -> species.code == code)
        |> List.map markerForSiteSpecies


markerForSiteSpecies : ( Site, Species ) -> Marker
markerForSiteSpecies ( site, species ) =
    let
        popupContents =
            "<div class=\"content\">"
                ++ "<h1 class=\"title is-6\">"
                ++ site.name
                ++ "</h1>"
                ++ "<h2 class=\"subtitle is-7 has-text-weight-normal\">"
                ++ speciesCodeToString species.code
                ++ "</h2>"
                ++ "<ul>"
                ++ "<li>Air quality index: "
                ++ species.airQualityIndex
                ++ "</li>"
                ++ "<li>Air quality band: "
                ++ airQualityBandToString species.airQualityBand
                ++ "</li>"
                ++ "</ul>"
                ++ "</div>"
    in
    { lat = site.lat
    , lng = site.lng
    , color = colorFromAirQualityBand species.airQualityBand
    , tooltip = site.name
    , popupContents = popupContents
    }


markerEncoder : Marker -> Json.Encode.Value
markerEncoder marker =
    Json.Encode.object
        [ ( "lat", Json.Encode.float marker.lat )
        , ( "lng", Json.Encode.float marker.lng )
        , ( "iconUrl", iconUrlEncoder marker.color )
        , ( "title", Json.Encode.string marker.tooltip )
        , ( "popupContents", Json.Encode.string marker.popupContents )
        ]


type MarkerColor
    = Green
    | Yellow
    | Red
    | Black


colorFromAirQualityBand : AirQualityBand -> MarkerColor
colorFromAirQualityBand band =
    case band of
        Low ->
            Green

        Moderate ->
            Yellow

        High ->
            Red

        VeryHigh ->
            Black


iconUrlEncoder : MarkerColor -> Json.Encode.Value
iconUrlEncoder markerColor =
    case markerColor of
        Green ->
            Json.Encode.string "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png"

        Yellow ->
            Json.Encode.string "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-gold.png"

        Red ->
            Json.Encode.string "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png"

        Black ->
            Json.Encode.string "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-black.png"



-- Msg & update


type MapMsg
    = GetDailyAirQualityData SpeciesCode
    | GotDailyAirQualityData SpeciesCode (Result Http.Error DailyAirQualityData)
    | UpdateMarkers SpeciesCode


updateMap : MapMsg -> MapModel -> ( MapModel, Cmd MapMsg )
updateMap msg map =
    case msg of
        GetDailyAirQualityData speciesCode ->
            ( map, getDailyAirQualityData speciesCode )

        GotDailyAirQualityData speciesCode result ->
            case result of
                Ok dailyAirQualityData ->
                    let
                        newMap =
                            { map | data = Loaded dailyAirQualityData }
                    in
                    ( newMap, updateMarkers newMap speciesCode )

                Err (Http.BadBody bodyError) ->
                    ( { map | data = Error ("Encountered bad body error: " ++ bodyError) }
                    , Cmd.none
                    )

                Err (Http.BadStatus status) ->
                    ( { map | data = Error ("Encountered bad status error: " ++ String.fromInt status) }
                    , Cmd.none
                    )

                Err (Http.BadUrl url) ->
                    ( { map | data = Error ("Encountered bad URL error: " ++ url) }
                    , Cmd.none
                    )

                Err Http.NetworkError ->
                    ( { map | data = Error "Encountered network error" }
                    , Cmd.none
                    )

                Err Http.Timeout ->
                    ( { map | data = Error "Encountered timeout error" }
                    , Cmd.none
                    )

        UpdateMarkers speciesCode ->
            ( map, updateMarkers map speciesCode )


getDailyAirQualityData : SpeciesCode -> Cmd MapMsg
getDailyAirQualityData speciesCode =
    -- Note that data is fetched for all species of pollutants, regardless of speciesCode which
    -- is only passed as the first argument to GotDailyAirQualityData message to select the
    -- species for which to display map markers.
    Http.get
        { url = dailyAirQualityDataUrl
        , expect = Http.expectJson (GotDailyAirQualityData speciesCode) dailyAirQualityDataDecoder
        }


dailyAirQualityDataUrl : String
dailyAirQualityDataUrl =
    "https://api.erg.ic.ac.uk/AirQuality/Daily/MonitoringIndex/Latest/GroupName=London/Json"


updateMarkers : MapModel -> SpeciesCode -> Cmd msg
updateMarkers map speciesCode =
    let
        markers =
            case map.data of
                Loaded data ->
                    markersFromData data speciesCode

                _ ->
                    []
    in
    Json.Encode.list markerEncoder markers
        |> Leaflet.resetMarkers



-- View


viewMap : Html msg
viewMap =
    div [ id "map", style "min-height" "90vh", style "z-index" "0" ] []
