module AoC2019.Day3.Puzzle exposing (part1, part2)

import AoC2019.Day3.Input exposing (input)
import Debug


type alias Point =
    ( Int, Int )


type alias Line =
    ( Point, Point )


type alias Direction =
    String


type alias WirePiece =
    ( Direction, Int )


parsedInput =
    String.lines input |> List.map parseWire


parseWire : String -> List Line
parseWire wire =
    let
        -- Gets ["R8", "D3", etc]
        pieces =
            String.split "," wire

        parsed =
            pieces |> List.map parseWirePiece

        firstPiece =
            List.head parsed |> Maybe.withDefault ( "", 0 )

        restPieces =
            List.tail parsed |> Maybe.withDefault []

        secondPoint =
            vectorizeWirePiece firstPiece

        firstLine =
            ( ( 0, 0 ), secondPoint )
    in
    List.foldl addWirePiece (List.singleton firstLine) restPieces


parseWirePiece : String -> WirePiece
parseWirePiece pieceStr =
    let
        dir =
            String.left 1 pieceStr

        amount =
            String.dropLeft 1 pieceStr |> (String.toInt >> Maybe.withDefault 0)
    in
    ( dir, amount )


vectorizeWirePiece : WirePiece -> Point
vectorizeWirePiece piece =
    let
        ( dir, amount ) =
            piece
    in
    if dir == "R" then
        ( amount, 0 )

    else if dir == "L" then
        ( -amount, 0 )

    else if dir == "U" then
        ( 0, amount )

    else
        ( 0, -amount )


addWirePiece : WirePiece -> List Line -> List Line
addWirePiece piece lines =
    let
        connecting =
            List.head lines |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )

        lastPoint =
            Tuple.second connecting

        ( lastX, lastY ) =
            lastPoint

        ( horAmount, vertAmount ) =
            vectorizeWirePiece piece

        newPoint =
            ( lastX + horAmount, lastY + vertAmount )

        newLine =
            ( lastPoint, newPoint )
    in
    newLine :: lines


lineIntersection : Line -> Line -> Maybe Point
lineIntersection line1 line2 =
    --https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            line1

        ( ( x3, y3 ), ( x4, y4 ) ) =
            line2

        bottom =
            ((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)) |> toFloat

        uTop =
            ((x1 - x2) * (y1 - y3)) - ((y1 - y2) * (x1 - x3)) |> toFloat

        tTop =
            ((x1 - x3) * (y3 - y4)) - ((y1 - y3) * (x3 - x4)) |> toFloat

        t =
            tTop / bottom

        u =
            -uTop / bottom

        p1 =
            toFloat x3 + u * ((x4 - x3) |> toFloat)

        p2 =
            toFloat y3 + u * ((y4 - y3) |> toFloat)
    in
    if u < 0.0 || u > 1.0 || t < 0.0 || t > 1.0 then
        Nothing

    else
        Just ( round p1, round p2 )


findCrossings : List Line -> Line -> List Point
findCrossings lineList line1 =
    lineList |> List.filterMap (lineIntersection line1)


getLines : List (List Line) -> ( List Line, List Line )
getLines x =
    case x of
        [ l1, l2 ] ->
            ( l1, l2 )

        _ ->
            ( [], [] ) |> Debug.log "Parsed input did not have 2 elements"



-- Part 1 answer: 399


part1 : String
part1 =
    let
        ( lineList1, lineList2 ) =
            getLines parsedInput

        allIntersections =
            lineList1
                |> List.concatMap (findCrossings lineList2)

        distances =
            allIntersections |> List.map (\( x, y ) -> abs x + abs y)
    in
    distances
        |> List.minimum
        |> Maybe.withDefault -1
        |> String.fromInt


part2 : String
part2 =
    "abc"
