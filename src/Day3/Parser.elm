module Day3.Parser exposing (ParsedSquare, parsedInput)

import Day3.Input exposing (input)
import Parser exposing ((|.), (|=), Parser)


type alias ParsedSquare =
    { id : Int
    , left : Int
    , top : Int
    , width : Int
    , height : Int
    }


parsedInput : List ParsedSquare
parsedInput =
    String.lines input
        |> List.map parseSquare
        |> List.filterMap Result.toMaybe


parseSquare : String -> Result (List Parser.DeadEnd) ParsedSquare
parseSquare str =
    Parser.run squareParser str


squareParser : Parser.Parser ParsedSquare
squareParser =
    Parser.succeed ParsedSquare
        |. Parser.symbol "#"
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "@"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol "x"
        |= Parser.int
