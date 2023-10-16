module Header exposing (..)

import Html exposing (Html, a, div, h1, i, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (onClick)
import Map exposing (MapMsg(..), SpeciesCode(..), allSpeciesCode, speciesCodeToString)
import Task



-- Model & init


type alias HeaderModel =
    { species : SpeciesCode
    }


initHeader : HeaderModel
initHeader =
    { species = NO2
    }



-- Msg & update


type HeaderMsg
    = SelectSpecies SpeciesCode
    | EmitMapMsg MapMsg


updateHeader : HeaderMsg -> HeaderModel -> ( HeaderModel, Cmd HeaderMsg )
updateHeader msg header =
    case msg of
        SelectSpecies species ->
            ( { header | species = species }
            , UpdateMarkers species |> Task.succeed |> Task.perform EmitMapMsg
            )

        EmitMapMsg _ ->
            -- This message is handled upstream, in Main.update
            ( header, Cmd.none )



-- View


viewHeader : HeaderModel -> Html HeaderMsg
viewHeader header =
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
                                [ text (speciesCodeToString header.species) ]
                            , div [ class "navbar-dropdown" ]
                                (allSpeciesCode |> List.map (viewDropdownItem header))
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


viewDropdownItem : HeaderModel -> SpeciesCode -> Html HeaderMsg
viewDropdownItem header code =
    if header.species == code then
        div [ class "navbar-item has-text-weight-semibold" ] [ text (speciesCodeToString code) ]

    else
        a [ class "navbar-item", onClick (SelectSpecies code) ] [ text (speciesCodeToString code) ]
