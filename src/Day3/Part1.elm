module Day3.Part1 exposing (solution)

import Day3.Parser exposing (ParsedSquare, parsedInput)
import Dict
import Maybe
import Utils.Dict as DictUtils
import Utils.List exposing (cartesian)
import Utils.Maybe exposing (exists)


type alias Coord =
    ( Int, Int )


type alias Int2dTable =
    Dict.Dict Int (Dict.Dict Int Int)


addCoord : Coord -> Int2dTable -> Int2dTable
addCoord ( x, y ) table =
    let
        col =
            Dict.get x table |> Maybe.withDefault Dict.empty

        newCol =
            DictUtils.addToDict y col

        newTable =
            Dict.insert x newCol table
    in
    newTable


addCoords : Int2dTable -> List Coord -> Int2dTable
addCoords table list =
    List.foldl addCoord table list


toCoords : ParsedSquare -> List Coord
toCoords square =
    let
        left =
            square.left

        top =
            square.top

        xs =
            List.range left (left + square.width - 1)

        ys =
            List.range top (top + square.height - 1)
    in
    cartesian xs ys


countMarked : Int2dTable -> Int
countMarked table =
    let
        cols =
            Dict.values table

        allValues =
            List.concatMap Dict.values cols
    in
    List.filter (\n -> n > 1) allValues |> List.length



-- Part 1 solution: 112418


solution : String
solution =
    let
        coords =
            parsedInput |> List.concatMap toCoords

        table =
            addCoords Dict.empty coords
    in
    countMarked table |> String.fromInt
