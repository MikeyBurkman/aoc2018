module Main exposing (Msg(..), main, update, view)

import AoC2019.Day3.Puzzle as Solution
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import List
import Maybe


main =
    Browser.sandbox { init = "", update = update, view = view }


type Puzzle
    = Part1
    | Part2


type Msg
    = Selected Puzzle


update msg model =
    case msg of
        Selected Part1 ->
            Solution.part1

        Selected Part2 ->
            Solution.part2


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
                Part1
                "Part 1"
            , puzzleBtn
                Part2
                "Part 2"
            ]
        ]
