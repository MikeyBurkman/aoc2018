module Utils.Dict exposing (addToDict, toString)

import Dict


{-| For an accumulating dictionary.
addToDict ["a": 1, "b": 2] "a" == ["a": 2, "b": 2]
addToDict ["a": 1] "b" == ["a": 1, "b": 1]
-}
addToDict : comparable -> Dict.Dict comparable Int -> Dict.Dict comparable Int
addToDict n dict =
    let
        count =
            Dict.get n dict |> Maybe.withDefault 0
    in
    Dict.insert n (count + 1) dict


toString : (k -> String) -> (v -> String) -> Dict.Dict k v -> String
toString keyToString valueToString table =
    Dict.toList table
        |> List.map (\( k, v ) -> "(" ++ keyToString k ++ ", " ++ valueToString v ++ ")")
        |> String.join ", "
        |> (\s -> "[" ++ s ++ "]")
