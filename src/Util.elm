module Util exposing (conditionallyPick)


conditionallyPick : a -> a -> Bool -> a
conditionallyPick a b c =
    if c then
        a

    else
        b
