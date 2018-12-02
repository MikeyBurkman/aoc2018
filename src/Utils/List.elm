module Utils.List exposing (find, zip)

import List


zip : List m -> List n -> List ( m, n )
zip l1 l2 =
    List.map2 Tuple.pair l1 l2


find : (m -> Bool) -> List m -> Maybe m
find test list =
    list |> List.filter test |> List.head