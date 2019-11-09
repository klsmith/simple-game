module ListUtil exposing (..)


appendMaybe : List a -> Maybe a -> List a
appendMaybe list maybe =
    Maybe.map List.singleton maybe
        |> Maybe.withDefault []
        |> (++) list
