module Header exposing (..)

import Map exposing (MapMsg(..), SpeciesCode(..))
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
