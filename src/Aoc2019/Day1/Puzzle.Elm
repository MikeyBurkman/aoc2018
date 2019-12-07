module AoC2019.Day1.Puzzle exposing (part1, part2)

import AoC2019.Day1.Input exposing (input)


parsedInput =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault 0)


fuelRequired : Int -> Int
fuelRequired mass =
    mass // 3 - 2


fuelRequiredPt2 : Int -> Int -> Int
fuelRequiredPt2 start mass =
    let
        m =
            fuelRequired mass
    in
    if m > 0 then
        fuelRequiredPt2 (start + m) m

    else
        start



-- Part 1 answer: 3434390


part1 : String
part1 =
    parsedInput |> List.map fuelRequired |> List.sum |> String.fromInt



-- Part 2 answer: 5148724


part2 : String
part2 =
    parsedInput |> List.map (fuelRequiredPt2 0) |> List.sum |> String.fromInt
