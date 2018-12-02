module Day2.Puzzle exposing (part1)

import Day2.Input exposing (input)
import Dict
import List
import Maybe
import Utils.Dict exposing (addToDict)


parsedInput =
    String.lines input



-- Part 1 solution: 5368


mapLetters : String -> Dict.Dict Char Int
mapLetters str =
    String.foldl addToDict Dict.empty str


hasCount : Int -> Dict.Dict Char Int -> Bool
hasCount count dict =
    Dict.values dict |> List.any ((==) count)


part1 : String
part1 =
    let
        mapped =
            List.map mapLetters parsedInput

        withTwo =
            List.filter (hasCount 2) mapped

        withThree =
            List.filter (hasCount 3) mapped

        checksum =
            List.length withTwo * List.length withThree
    in
    String.fromInt checksum
