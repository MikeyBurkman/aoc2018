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


type alias Program =
    Array.Array Int


getAt : Int -> Program -> Int
getAt position program =
    Array.get position program |> Maybe.withDefault 0


type alias ParsedOps =
    { input1 : Int, input2 : Int, resultPos : Int }


getParsedOps : Int -> Program -> ParsedOps
getParsedOps pos program =
    let
        input1Pos =
            getAt (pos + 1) program

        input1 =
            getAt input1Pos program

        input2Pos =
            getAt (pos + 2) program

        input2 =
            getAt input2Pos program

        resultPos =
            getAt (pos + 3) program
    in
    { input1 = input1, input2 = input2, resultPos = resultPos }


runOp : Int -> Program -> Program
runOp pos program =
    let
        opcode =
            getAt pos program
    in
    if opcode == 99 then
        program

    else
        let
            parsedOps =
                getParsedOps pos program

            operand =
                if opcode == 1 then
                    (+)

                else
                    (*)

            result =
                operand parsedOps.input1 parsedOps.input2
        in
        Array.set parsedOps.resultPos result program
            |> runOp (pos + 4)


getOutput : Int -> Int -> Program -> Int
getOutput noun verb program =
    program
        |> Array.set 1 noun
        |> Array.set 2 verb
        |> runOp 0
        |> Array.get 0
        |> Maybe.withDefault 0


findMatching : Int -> Program -> ( Int, Int )
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
