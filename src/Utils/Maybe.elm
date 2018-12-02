module Utils.Maybe exposing (exists)


exists : Maybe m -> Bool
exists maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
