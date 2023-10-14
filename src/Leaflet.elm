port module Leaflet exposing (..)

import Json.Encode exposing (Value)


port resetMarkers : Value -> Cmd msg
