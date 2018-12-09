module Day7.Puzzle exposing (part1, part2)

import Day7.Input as Input
import Day7.Parser exposing (ParsedNode, parse)
import Debug
import Dict
import Maybe
import Set


type alias Dag =
    Dict.Dict Char (Set.Set Char)


parsedInput =
    Input.input |> parse


addToDag : ParsedNode -> Dag -> Dag
addToDag node dag =
    let
        set =
            Dict.get node.id dag |> Maybe.withDefault Set.empty

        newSet =
            Set.insert node.dep set
    in
    Dict.insert node.id newSet dag


turnIntoDag : List ParsedNode -> Dag
turnIntoDag list =
    List.foldl addToDag Dict.empty list


getAllNodeIds : Dag -> Set.Set Char
getAllNodeIds dag =
    List.foldr Set.union Set.empty (Dict.values dag)


normalizeDag : Dag -> Dag
normalizeDag dag =
    let
        allNodes =
            getAllNodeIds dag

        dagKeys =
            dag |> Dict.keys |> Set.fromList

        notFound =
            Set.diff allNodes dagKeys
    in
    Set.foldl (\c -> Dict.insert c Set.empty) dag notFound


removeDependency : Dag -> Char -> Dag
removeDependency dag c =
    let
        withoutC =
            Dict.remove c dag
    in
    Dict.map (\k set -> Set.remove c set) withoutC


firstNoDepNode : Dag -> Maybe Char
firstNoDepNode dag =
    let
        empty =
            Dict.filter (\c set -> Set.isEmpty set) dag
    in
    Dict.keys empty |> List.sort |> List.head


processDependencies : Dag -> List Char -> List Char
processDependencies dag depsSoFar =
    let
        noDepNode =
            firstNoDepNode dag |> Debug.log "Next nodep"
    in
    case noDepNode of
        Nothing ->
            depsSoFar

        Just node ->
            processDependencies (removeDependency dag node) (depsSoFar ++ [ node ])


part1 : String
part1 =
    let
        dag =
            parsedInput |> turnIntoDag |> normalizeDag |> Debug.log "dag"
    in
    processDependencies dag [] |> String.fromList


part2 : String
part2 =
    "def"
