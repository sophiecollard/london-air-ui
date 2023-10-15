module Header exposing (..)

import Map exposing (SpeciesCode(..))



-- Model & init


type alias HeaderModel =
    { species : SpeciesCode
    }


initHeader : HeaderModel
initHeader =
    { species = NO2
    }
