module AoC2018.Day1.Puzzle exposing (part1, part2)

import AoC2018.Day1.Input exposing (input)
import List
import Maybe
import Set
import Utils.Generator as Gen


parsedInput =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault 0)



-- Part 1 Solution: 510


part1 : String
part1 =
    List.sum parsedInput |> String.fromInt



-- Part 2 solution: 69074


part2 : String
part2 =
    findDuplicateFreq (Gen.fromList parsedInput 0) Set.empty 0 |> String.fromInt


findDuplicateFreq : Gen.Generator Int -> Set.Set Int -> Int -> Int
findDuplicateFreq generator seen curFreq =
    let
        newFreq =
            curFreq + Gen.current generator
    in
    if Set.member newFreq seen then
        newFreq

    else
        findDuplicateFreq (Gen.next generator) (Set.insert newFreq seen) newFreq
