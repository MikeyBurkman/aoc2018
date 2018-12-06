module Day5.Puzzle exposing (part1, part2)

import Array
import Day5.Input as Input
import Debug
import Maybe
import Set
import Utils.Array as UtilsArray


input =
    Input.testInput


type alias Polymer =
    { chars : Array.Array Char
    , reacted : Set.Set Int
    }


toPolymer : List Char -> Polymer
toPolymer charList =
    { chars = Array.fromList charList, reacted = Set.empty }


parsedInput : Polymer
parsedInput =
    String.toList input |> toPolymer


isReactive : Char -> Char -> Bool
isReactive c1 c2 =
    Char.toUpper c1 == Char.toUpper c2


reducePolymer : Int -> Int -> Polymer -> Polymer
reducePolymer idx1 idx2 polymer =
    let
        after1 =
            Set.insert idx1 polymer.reacted

        after2 =
            Set.insert idx2 after1
    in
    { polymer | reacted = after2 }


nextAvailable : Polymer -> Int -> Maybe ( Int, Char )
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


getElementPair : Int -> Polymer -> Maybe ( ( Int, Char ), ( Int, Char ) )
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


react : Int -> Polymer -> Polymer
react idx polymer =
    let
        pair =
            getElementPair idx polymer

        maybeReactive =
            Maybe.map (\( ( _, c1 ), ( _, c2 ) ) -> isReactive c1 c2) pair
    in
    case ( pair, maybeReactive ) of
        ( Just ( ( i1, _ ), ( i2, _ ) ), Just True ) ->
            reducePolymer i1 i2 polymer |> react 0

        ( _, Just False ) ->
            react (idx + 1) polymer

        _ ->
            polymer


polymerToString : Polymer -> String
polymerToString polymer =
    Array.toIndexedList polymer.chars
        |> List.filter (\( i, _ ) -> (not << Set.member i) polymer.reacted)
        |> List.map Tuple.second
        |> String.fromList


part1 : String
part1 =
    parsedInput |> react 0 |> Debug.log "Solution" |> polymerToString


part2 : String
part2 =
    "abc"
