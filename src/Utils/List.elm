module Utils.List exposing (..)


flatMap : (a -> List b) -> List a -> List b
flatMap f list =
    List.map f list |> List.concat
