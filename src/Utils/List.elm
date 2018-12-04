module Utils.List exposing (cartesian, find, zip)

import List


cartesian : List m -> List n -> List ( m, n )
cartesian l1 l2 =
    List.concatMap (\m -> List.map (\n -> ( m, n )) l2) l1


zip : List m -> List n -> List ( m, n )
zip l1 l2 =
    List.map2 Tuple.pair l1 l2


find : (m -> Bool) -> List m -> Maybe m
find test list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if test head then
                Just head

            else
                find test tail
