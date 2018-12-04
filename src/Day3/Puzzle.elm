module Day3.Puzzle exposing (part1, part2)

import Day3.Parser exposing (ParsedSquare, parsedInput)
import Dict
import Maybe
import Utils.Dict as DictUtils
import Utils.List exposing (cartesian, find)
import Utils.Maybe exposing (maybeIs)


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


buildTableFromInput : Int2dTable
buildTableFromInput =
    parsedInput
        |> List.concatMap toCoords
        |> addCoords Dict.empty



-- Part 1 solution: 112418


part1 : String
part1 =
    let
        table =
            buildTableFromInput
    in
    countMarked table |> String.fromInt



-- Part 2 solution: 560


coordIs : Int2dTable -> Int -> Coord -> Bool
coordIs table match ( x, y ) =
    let
        col =
            Dict.get x table
    in
    case col of
        Nothing ->
            False

        Just row ->
            Dict.get y row |> maybeIs match


allCoordsSetTo : Int2dTable -> Int -> List Coord -> Bool
allCoordsSetTo table match coords =
    List.all (coordIs table match) coords


parsedSquareNoOverlap : Int2dTable -> ParsedSquare -> Bool
parsedSquareNoOverlap table square =
    toCoords square |> allCoordsSetTo table 1


part2 : String
part2 =
    let
        table =
            buildTableFromInput

        match =
            find (parsedSquareNoOverlap table) parsedInput
    in
    case match of
        Nothing ->
            "Uhoh..."

        Just square ->
            square.id |> String.fromInt
