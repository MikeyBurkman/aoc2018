module Utils.Generator exposing (Generator, current, fromArray, fromList, next)

import Array
import List
import Maybe



{- Creates an infinite generator from a list.
   It will keep cycling throught the list, repeating form the beginning if it reaches the end.
   Note the `defVal` is required by the type system but will never be used unless the source is empty.
-}


type alias Generator n =
    { arr : Array.Array n
    , idx : Int
    , defVal : n
    }


next : Generator n -> Generator n
next generator =
    let
        idx =
            remainderBy (Array.length generator.arr) (generator.idx + 1)
    in
    { generator | idx = idx }


current : Generator n -> n
current generator =
    Array.get generator.idx generator.arr |> Maybe.withDefault generator.defVal


fromList : List n -> n -> Generator n
fromList list defVal =
    fromArray (Array.fromList list) defVal


fromArray : Array.Array n -> n -> Generator n
fromArray arr defVal =
    { arr = arr
    , idx = 0
    , defVal = defVal
    }
