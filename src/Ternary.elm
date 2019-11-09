module Ternary exposing (tern)


tern : Bool -> a -> a -> a
tern bool trueResult falseResult =
    if bool then
        trueResult

    else
        falseResult
