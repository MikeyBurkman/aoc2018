module Day2.Puzzle exposing (part1, part2)

import Day2.Input exposing (input)
import Dict
import List
import Maybe
import Utils.Dict exposing (addToDict)
import Utils.List exposing (find, zip)


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



-- Part 2 solution: cvgywxqubnuaefmsljdrpfzyi


zipStrings : String -> String -> List ( Char, Char )
zipStrings s1 s2 =
    zip (String.toList s1) (String.toList s2)


pairEqual : ( a, a ) -> Bool
pairEqual tuple =
    Tuple.first tuple == Tuple.second tuple


countCharDiffs : String -> String -> Int
countCharDiffs s1 s2 =
    zipStrings s1 s2
        |> List.filter (pairEqual >> not)
        |> List.length


oneLetterOff : String -> String -> Bool
oneLetterOff s1 s2 =
    (s1 /= s2)
        && (countCharDiffs s1 s2 == 1)


findCloseMatch : List String -> String -> Maybe ( String, String )
findCloseMatch list str =
    let
        found =
            find (oneLetterOff str) list
    in
    case found of
        Just s ->
            Just ( s, str )

        Nothing ->
            Nothing


findCloseMatchInList : List String -> Maybe ( String, String )
findCloseMatchInList list =
    let
        search =
            findCloseMatch list
    in
    List.filterMap search list |> List.head


removeDifferingChars : String -> String -> String
removeDifferingChars s1 s2 =
    zipStrings s1 s2
        |> List.filter pairEqual
        |> List.map Tuple.first
        |> String.fromList


part2 : String
part2 =
    let
        found =
            findCloseMatchInList parsedInput
    in
    case found of
        Just ( s1, s2 ) ->
            removeDifferingChars s1 s2

        Nothing ->
            "Nothing found..."
