module Day1.Puzzle exposing (part1, part2)

import Array
import Day1.Input exposing (input)
import List
import Maybe
import Set


parsedInput =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault 0)


part1 : Int
part1 =
    List.foldl (+) 0 parsedInput


{-| Get the value out of an array, wrapping around to the front if idx > arr length
-}
nextVal : Array.Array Int -> Int -> Int
nextVal arr idx =
    let
        i =
            remainderBy (Array.length arr) idx
    in
    Array.get i arr |> Maybe.withDefault 0


findDuplicateFreq : Set.Set Int -> Array.Array Int -> Int -> Int -> Int
findDuplicateFreq seen values curFreq curIdx =
    let
        val =
            nextVal values curIdx

        newFreq =
            curFreq + val
    in
    if Set.member newFreq seen then
        newFreq

    else
        findDuplicateFreq (Set.insert newFreq seen) values newFreq (curIdx + 1)


part2 : Int
part2 =
    let
        arr =
            Array.fromList parsedInput
    in
    findDuplicateFreq Set.empty arr 0 0
