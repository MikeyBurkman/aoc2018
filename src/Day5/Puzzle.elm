module Day5.Puzzle exposing (part1, part2)

import Array
import Day5.Input exposing (input)
import Debug
import Maybe
import Utils.Array as UtilsArray


type alias Polymer =
    Array.Array Char


parsedInput =
    String.toList input |> Array.fromList


isReactive : ( Char, Char ) -> Bool
isReactive ( c1, c2 ) =
    Char.toUpper c1 == Char.toUpper c2


reducePolymer : Int -> Polymer -> Polymer
reducePolymer idx polymer =
    let
        head =
            Array.slice 0 idx polymer

        tail =
            Array.slice (idx + 2) (Array.length polymer) polymer
    in
    Array.append head tail


getElementPair : Int -> Polymer -> Maybe ( Char, Char )
getElementPair idx polymer =
    let
        first =
            Array.get idx polymer

        second =
            Array.get (idx + 1) polymer
    in
    case ( first, second ) of
        ( Just f, Just s ) ->
            Just ( f, s )

        _ ->
            Nothing


react : Int -> Polymer -> Polymer
react idx polymer =
    let
        maybeReactive =
            getElementPair idx polymer |> Maybe.map isReactive
    in
    case maybeReactive of
        Nothing ->
            polymer

        Just True ->
            reducePolymer idx polymer |> react 0

        Just False ->
            react (idx + 1) polymer


part1 : String
part1 =
    react 0 parsedInput |> UtilsArray.toString


part2 : String
part2 =
    "abc"
