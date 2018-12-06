module Day5.Puzzle exposing (part1, part2)

import Array
import Day5.Input as Input
import Debug
import Maybe
import Set
import Utils.Array as UtilsArray


input =
    Input.input


type alias Polymer =
    { chars : Array.Array Char
    , reacted : Set.Set Int
    }


type alias CharPos =
    ( Int, Char )


toPolymer : List Char -> Polymer
toPolymer charList =
    { chars = Array.fromList charList, reacted = Set.empty }


parsedInput : Polymer
parsedInput =
    String.toList input |> toPolymer


isReactive : Char -> Char -> Bool
isReactive c1 c2 =
    (Char.isUpper c1 && Char.isLower c2 && c1 == Char.toUpper c2)
        || (Char.isUpper c2 && Char.isLower c1 && c2 == Char.toUpper c1)


reducePolymer : Int -> Int -> Polymer -> Polymer
reducePolymer idx1 idx2 polymer =
    let
        after1 =
            Set.insert idx1 polymer.reacted

        after2 =
            Set.insert idx2 after1
    in
    { polymer | reacted = after2 }


nextAvailable : Polymer -> Int -> Maybe CharPos
nextAvailable polymer idx =
    if idx >= Array.length polymer.chars then
        Nothing

    else if Set.member idx polymer.reacted then
        nextAvailable polymer (idx + 1)

    else
        case Array.get idx polymer.chars of
            Just c ->
                Just ( idx, c )

            Nothing ->
                Nothing


getElementPair : Int -> Polymer -> Maybe ( CharPos, CharPos )
getElementPair idx polymer =
    let
        size =
            Array.length polymer.chars

        first =
            nextAvailable polymer idx

        second =
            Maybe.andThen (\( i, _ ) -> nextAvailable polymer (i + 1)) first
    in
    case ( first, second ) of
        ( Just x, Just y ) ->
            Just ( x, y )

        _ ->
            Nothing


nextIdx : Int -> Polymer -> Int
nextIdx idx polymer =
    if Set.member idx polymer.reacted then
        nextIdx (idx + 1) polymer

    else
        idx


prevIdx : Int -> Polymer -> Int
prevIdx idx polymer =
    if idx <= 0 then
        0

    else if Set.member idx polymer.reacted then
        prevIdx (idx - 1) polymer

    else
        idx


react : Int -> Polymer -> Polymer
react idx polymer =
    let
        pair =
            getElementPair idx polymer

        maybeReactive =
            Maybe.map (\( ( _, c1 ), ( _, c2 ) ) -> isReactive c1 c2) pair
    in
    case ( maybeReactive, pair ) of
        ( Just True, Just ( ( i1, _ ), ( i2, _ ) ) ) ->
            let
                reduced =
                    reducePolymer i1 i2 polymer

                newIdx =
                    prevIdx i1 reduced
            in
            react newIdx reduced

        ( Just False, _ ) ->
            react (nextIdx (idx + 1) polymer) polymer

        _ ->
            polymer


polymerToString : Polymer -> String
polymerToString polymer =
    Array.toIndexedList polymer.chars
        |> List.filter (\( i, _ ) -> (not << Set.member i) polymer.reacted)
        |> List.map Tuple.second
        |> String.fromList



-- Part 1 solution: 10886


part1 : String
part1 =
    parsedInput
        |> react 0
        |> polymerToString
        |> String.length
        |> String.fromInt


part2 : String
part2 =
    "abc"
