module Main exposing (main)

import Browser
import Element exposing (Element, centerX, column, el, fill, row, spacing, text, width)
import Element.Input exposing (button)
import Html exposing (Html)



-- MODEL


type Msg
    = YesButtonPressed
    | NoButtonPressed


type alias Model =
    Int



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ centerX, width fill ] <|
        column
            [ centerX ]
            [ el [ centerX ] <| text <| "20 Questions " ++ String.fromInt model
            , el [ centerX ] <| text "Is it bigger than a microwave?"
            , row [ centerX, spacing 50 ]
                [ button [] { onPress = Just YesButtonPressed, label = text "Yes" }
                , button [] { onPress = Just NoButtonPressed, label = text "No" }
                ]
            ]


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }
