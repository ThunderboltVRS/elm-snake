module Util exposing (valueEqual)

import Types exposing (..)


valueEqual : Maybe a -> Maybe a -> Bool
valueEqual mA mB =
    case mA of
        Nothing ->
            False

        Just a ->
            case mB of
                Nothing ->
                    False

                Just b ->
                    a == b
