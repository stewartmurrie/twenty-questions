module Main exposing (main)

import Browser
import Element exposing (Element, centerX, column, el, fill, row, spacing, text, width)
import Element.Input exposing (button)
import Html exposing (Html)



-- MODEL
-- 20Q I think is represented by a binary tree. Each node is a question, with YES or NO branches.
-- Leaf nodes represent answers?


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


type Msg
    = YesButtonPressed
    | NoButtonPressed


type alias Model =
    Tree String


init : Model
init =
    Node "a microwave" Empty Empty



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        YesButtonPressed ->
            model

        NoButtonPressed ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ centerX, width fill ] <|
        column
            [ centerX ]
            [ el [ centerX ] <| text <| "20 Questions"
            , el [ centerX ] <| text <| "Is it " ++ nodeToString model ++ "?"
            , row [ centerX, spacing 50 ]
                [ button [] { onPress = Just YesButtonPressed, label = text "Yes" }
                , button [] { onPress = Just NoButtonPressed, label = text "No" }
                ]
            ]


nodeToString : Tree String -> String
nodeToString tree =
    case tree of
        Empty ->
            "(empty)"

        Node n _ _ ->
            n



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
