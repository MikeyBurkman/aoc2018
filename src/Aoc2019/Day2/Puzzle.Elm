module AoC2019.Day2.Puzzle exposing (part1, part2)

import AoC2019.Day2.Input exposing (input)
import Array
import Maybe


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



-- Part 1 answer: 4484226


part1 : String
part1 =
    parsedInput
        |> Array.set 1 12
        |> Array.set 2 2
        |> runOp 0
        |> Array.get 0
        |> Maybe.withDefault 0
        |> String.fromInt


part2 : String
part2 =
    "abc"
