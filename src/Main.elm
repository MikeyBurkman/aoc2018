module Main exposing (Msg(..), main, update, view)

import Browser
import Day1.Puzzle as Day1
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import List
import Maybe


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Puzzle
    = Day1_1
    | Day1_2


type Msg
    = Selected Puzzle


update msg model =
    case msg of
        Selected Day1_1 ->
            Day1.part1

        Selected Day1_2 ->
            Day1.part2


puzzleBtn : Puzzle -> String -> Html Msg
puzzleBtn puzzle label =
    div [] [ button [ onClick (Selected puzzle) ] [ text label ] ]


view model =
    div []
        [ div [] [ text ("Solution: " ++ String.fromInt model) ]
        , br [] []
        , div
            []
            [ puzzleBtn
                Day1_1
                "Day 1 Part 1"
            , puzzleBtn
                Day1_2
                "Day 1 Part 2"
            ]
        ]
