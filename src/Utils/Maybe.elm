module Utils.Maybe exposing (exists, maybeIs)


exists : Maybe m -> Bool
exists maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


maybeIs : comparable -> Maybe comparable -> Bool
maybeIs val maybe =
    case maybe of
        Just v ->
            v == val

        Nothing ->
            False
