module Utils.String exposing (toCharArray)

import Array
import List


toCharArray : String -> Array.Array Char
toCharArray str =
    String.toList str |> Array.fromList
