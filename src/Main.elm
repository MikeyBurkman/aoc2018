module Main exposing (Msg(..), main, update, view)

import Browser
import Day1.Puzzle as Day1
import Day2.Puzzle as Day2
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import List
import Maybe


main =
    Browser.sandbox { init = "", update = update, view = view }


type Puzzle
    = Day1_1
    | Day1_2
    | Day2_1
    | Day2_2


type Msg
    = Selected Puzzle


update msg model =
    case msg of
        Selected Day1_1 ->
            Day1.part1

        Selected Day1_2 ->
            Day1.part2

        Selected Day2_1 ->
            Day2.part1

        Selected Day2_2 ->
            Day2.part2


puzzleBtn : Puzzle -> String -> Html Msg
puzzleBtn puzzle label =
    div [] [ button [ onClick (Selected puzzle) ] [ text label ] ]


view model =
    div []
        [ div [] [ text ("Solution: " ++ model) ]
        , br [] []
        , div
            []
            [ puzzleBtn
                Day1_1
                "Day 1 Part 1"
            , puzzleBtn
                Day1_2
                "Day 1 Part 2"
            , puzzleBtn
                Day2_1
                "Day 2 Part 1"
            , puzzleBtn
                Day2_2
                "Day 2 Part 2"
            ]
        ]
