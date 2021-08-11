module Main exposing (main)

import Browser
import Element exposing (Element, el, text)
import Html exposing (Html)


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


update : a -> a
update m =
    m


view : Int -> Html msg
view model =
    Element.layout []
        (text <| "20 Questions " ++ String.fromInt model)
