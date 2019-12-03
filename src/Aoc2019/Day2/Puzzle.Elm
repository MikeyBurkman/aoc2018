module AoC2019.Day2.Puzzle exposing (part1, part2)

import AoC2019.Day2.Input exposing (input)
import Array
import Maybe
import Utils.List exposing (cartesian, find)


parsedInput =
    input
        |> String.split ","
        |> List.map (String.toInt >> Maybe.withDefault 0)
        |> Array.fromList


type alias ParsedOps =
    { input1 : Int, input2 : Int, resultPos : Int }


getParsedOps : Int -> Array.Array Int -> ParsedOps
getParsedOps pos program =
    let
        input1Pos =
            Array.get (pos + 1) program |> Maybe.withDefault 0

        input1 =
            Array.get input1Pos program |> Maybe.withDefault 0

        input2Pos =
            Array.get (pos + 2) program |> Maybe.withDefault 0

        input2 =
            Array.get input2Pos program |> Maybe.withDefault 0

        resultPos =
            Array.get (pos + 3) program |> Maybe.withDefault 0
    in
    { input1 = input1, input2 = input2, resultPos = resultPos }


runOp : Int -> Array.Array Int -> Array.Array Int
runOp pos program =
    let
        opcode =
            Array.get pos program |> Maybe.withDefault 0
    in
    if opcode == 99 then
        program

    else
        let
            parsedOps =
                getParsedOps pos program
        in
        if opcode == 1 then
            Array.set parsedOps.resultPos (parsedOps.input1 + parsedOps.input2) program
                |> runOp (pos + 4)

        else
            Array.set parsedOps.resultPos (parsedOps.input1 * parsedOps.input2) program
                |> runOp (pos + 4)


getOutput : Int -> Int -> Array.Array Int -> Int
getOutput noun verb program =
    program
        |> Array.set 1 noun
        |> Array.set 2 verb
        |> runOp 0
        |> Array.get 0
        |> Maybe.withDefault 0


findMatching : Int -> Array.Array Int -> ( Int, Int )
findMatching goal program =
    let
        possible =
            cartesian (List.range 0 99) (List.range 0 99)

        match =
            possible |> find (\( noun, verb ) -> getOutput noun verb program == goal)
    in
    case match of
        Just result ->
            result

        Nothing ->
            ( -1, -1 )



-- Part 1 answer: 4484226


part1 : String
part1 =
    parsedInput
        |> getOutput 12 2
        |> String.fromInt



-- Part 2 answer: 5696


part2 : String
part2 =
    let
        ( noun, verb ) =
            findMatching 19690720 parsedInput
    in
    (noun * 100) + verb |> String.fromInt
