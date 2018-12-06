module Utils.List exposing (cartesian, find, maxBy, zip)

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


maxBy : (m -> Int) -> List m -> Maybe m
maxBy fn list =
    maxByHelper fn list Nothing


maxByHelper : (m -> Int) -> List m -> Maybe m -> Maybe m
maxByHelper fn list soFar =
    case list of
        [] ->
            soFar

        head :: tail ->
            case soFar of
                Nothing ->
                    maxByHelper fn tail (Just head)

                Just prevValue ->
                    if fn head > fn prevValue then
                        maxByHelper fn tail (Just head)

                    else
                        maxByHelper fn tail (Just prevValue)
