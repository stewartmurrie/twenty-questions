module Main exposing (main)

import Browser
import Html exposing (div)


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


update m =
    m


view model =
    div [] []
