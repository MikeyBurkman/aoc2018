module Utils.Array exposing (zip)

import Array
import List


zip : List m -> List n -> Array.Array ( m, n )
zip l1 l2 =
    List.map2 Tuple.pair l1 l2 |> Array.fromList
