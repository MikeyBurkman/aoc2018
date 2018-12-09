module Day7.Parser exposing (ParsedNode, parse)

import Debug
import Maybe
import Regex


type alias ParsedNode =
    { id : Char
    , dep : Char
    }


regex =
    Regex.fromString "Step (.) must be finished before step (.) can begin."
        |> Maybe.withDefault Regex.never


parse : String -> List ParsedNode
parse str =
    String.lines str |> List.map parseNode


parseNode : String -> ParsedNode
parseNode str =
    let
        x =
            Regex.find regex str |> List.head
    in
    case x of
        Just head ->
            fromStringList head.submatches

        Nothing ->
            ParsedNode 'x' 'x'


maybeStringToChar : Maybe String -> Char
maybeStringToChar s =
    case s of
        Just s2 ->
            String.toList s2 |> List.head |> Maybe.withDefault 'x'

        Nothing ->
            Debug.todo "bad input"


fromStringList : List (Maybe String) -> ParsedNode
fromStringList list =
    case list of
        dep :: id :: rest ->
            ParsedNode (maybeStringToChar id) (maybeStringToChar dep)

        _ ->
            Debug.todo "why is this such a pain in the ass"
