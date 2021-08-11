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


type State
    = Running
    | Won
    | Lost


type Msg
    = YesButtonPressed
    | NoButtonPressed


type alias Model =
    { tree : Tree String
    , currentNode : Tree String
    , state : State
    }


initialTree : Tree String
initialTree =
    Node "Does it star Arnold Swarzenegger?" (Node "RoboCop" Empty Empty) (Node "The Terminator" Empty Empty)


init : Model
init =
    { tree = initialTree, currentNode = initialTree, state = Running }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        YesButtonPressed ->
            case model.currentNode of
                Empty ->
                    { model | currentNode = Empty }

                Node _ _ r ->
                    case r of
                        Empty ->
                            { model | state = Won }

                        Node _ _ _ ->
                            { model | currentNode = r }

        NoButtonPressed ->
            case model.currentNode of
                Empty ->
                    { model | currentNode = Empty }

                Node _ l _ ->
                    case l of
                        Empty ->
                            { model | state = Lost }

                        Node _ _ _ ->
                            { model | currentNode = l }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ centerX, width fill ] <|
        column
            [ centerX ]
            [ el [ centerX ] <| text <| "20 Movie Questions"
            , case model.state of
                Won ->
                    text "Yay! I guessed right!"

                Lost ->
                    text "Bummer! I got it wrong."

                -- TODO: ask questions to grow the knowledge graph
                Running ->
                    column []
                        [ case model.currentNode of
                            Empty ->
                                text "This shouldn't happen!"

                            Node n l r ->
                                case l of
                                    Empty ->
                                        text <| "Is it " ++ n ++ "?"

                                    _ ->
                                        text n
                        , row [ centerX, spacing 50 ]
                            [ button [] { onPress = Just YesButtonPressed, label = text "Yes" }
                            , button [] { onPress = Just NoButtonPressed, label = text "No" }
                            ]
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
