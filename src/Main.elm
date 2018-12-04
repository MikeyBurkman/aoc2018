module Main exposing (Msg(..), main, update, view)

import Browser
import Day3.Puzzle as Day3Part1
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import List
import Maybe


main =
    Browser.sandbox { init = "", update = update, view = view }


type Puzzle
    = Day3_1
    | Day3_2


type Msg
    = Selected Puzzle


update msg model =
    case msg of
        Selected Day3_1 ->
            Day3Part1.part1

        Selected Day3_2 ->
            Day3Part1.part2


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
                Day3_1
                "Day 3 Part 1"
            , puzzleBtn
                Day3_2
                "Day 3 Part 2"
            ]
        ]
