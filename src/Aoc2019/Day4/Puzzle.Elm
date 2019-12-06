module AoC2019.Day4.Puzzle exposing (part1, part2)

import AoC2019.Day4.Input exposing (input)
import Debug
import Dict
import Set
import Utils.Dict exposing (addToDict)


parsedInput =
    let
        values =
            input
                |> String.split "-"
                |> List.filterMap String.toInt
    in
    case values of
        [ start, end ] ->
            List.range start end |> List.map String.fromInt

        _ ->
            [ "" ] |> Debug.log "Parsed input did not have 2 elements"


hasAdjacentDigits : Set.Set Char -> List Char -> Bool
hasAdjacentDigits seen str =
    case str of
        [] ->
            False

        head :: rest ->
            if Set.member head seen then
                True

            else
                hasAdjacentDigits (Set.insert head seen) rest


digitsIncrease : Int -> List Int -> Bool
digitsIncrease previous str =
    case str of
        [] ->
            True

        head :: rest ->
            if head < previous then
                False

            else
                digitsIncrease head rest


hasOneTwiceMatchingNumber : Dict.Dict Char Int -> List Char -> Bool
hasOneTwiceMatchingNumber counts str =
    case str of
        [] ->
            Dict.values counts |> List.any (\x -> x == 2)

        head :: rest ->
            hasOneTwiceMatchingNumber (addToDict head counts) rest


couldBePassword : String -> Bool
couldBePassword str =
    let
        x =
            str
                |> String.toList
                |> hasAdjacentDigits Set.empty

        y =
            str
                |> String.split ""
                |> List.filterMap String.toInt
                |> digitsIncrease 0
    in
    x && y


couldBePassword2 : String -> Bool
couldBePassword2 str =
    let
        x =
            couldBePassword str

        y =
            str
                |> String.toList
                |> hasOneTwiceMatchingNumber Dict.empty
    in
    x && y



-- part 1 answer: 945


part1 : String
part1 =
    parsedInput
        |> List.filter couldBePassword
        |> List.length
        |> String.fromInt



-- Part 2 answer: 617


part2 : String
part2 =
    parsedInput
        |> List.filter couldBePassword2
        |> List.length
        |> String.fromInt
