module AoC2018.Day5.Puzzle exposing (part1, part2)

import AoC2018.Day5.Input as Input
import Array
import Debug
import Dict
import Maybe
import Set
import Utils.List exposing (maxBy)


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


parseInput : String -> Polymer
parseInput str =
    String.toList str |> toPolymer


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
    in
    case pair of
        Nothing ->
            polymer

        Just ( ( i1, c1 ), ( i2, c2 ) ) ->
            if isReactive c1 c2 then
                let
                    reduced =
                        reducePolymer i1 i2 polymer

                    newIdx =
                        prevIdx i1 reduced
                in
                react newIdx reduced

            else
                react (nextIdx (idx + 1) polymer) polymer


polymerToString : Polymer -> String
polymerToString polymer =
    Array.toIndexedList polymer.chars
        |> List.filter (\( i, _ ) -> (not << Set.member i) polymer.reacted)
        |> List.map Tuple.second
        |> String.fromList


withCharRemoved : Char -> String -> String
withCharRemoved char str =
    String.filter (\c -> c /= char && c /= Char.toUpper char) str


charsToRemove : List Char
charsToRemove =
    String.toList "abcdefghijklmnopqrstuvwxyz"


polymerLength : String -> Int
polymerLength str =
    str
        |> parseInput
        |> react 0
        |> polymerToString
        |> String.length


calcPolymerLengths : String -> List Char -> Dict.Dict Char Int -> Dict.Dict Char Int
calcPolymerLengths origInput charList results =
    case charList of
        [] ->
            results

        head :: tail ->
            let
                count =
                    origInput |> withCharRemoved head |> polymerLength

                newResults =
                    Dict.insert head count results
            in
            calcPolymerLengths origInput tail newResults



-- Part 1 solution: 10886


part1 : String
part1 =
    polymerLength input |> String.fromInt



-- Part 2 solution : 4684 (char = v)


part2 : String
part2 =
    let
        results =
            calcPolymerLengths input charsToRemove Dict.empty

        tuples =
            Dict.toList results

        max =
            maxBy (\( char, count ) -> -count) tuples
    in
    case max of
        Nothing ->
            "oop..."

        Just ( char, count ) ->
            let
                x =
                    Debug.log "Char + Count" ( char, count )
            in
            String.fromInt count
