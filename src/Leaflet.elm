port module Leaflet exposing (..)

import Json.Encode exposing (Value)


port resetMarkers : Value -> Cmd msg


port incoming : (String -> msg) -> Sub msg
