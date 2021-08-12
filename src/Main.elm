module Main exposing (main)

import Browser
import Element exposing (centerX, column, el, fill, row, spacing, text, width)
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import QuestionTree exposing (Answer(..), QuestionTree(..), addNode)



-- MODEL
-- 20Q I think is represented by a binary tree. Each node is a question, with YES or NO branches.
-- Leaf nodes represent answers?


type State
    = Running
    | Won
    | Lost
    | GotMovie
    | GotQuestion


type Msg
    = YesButtonPressed
    | NoButtonPressed
    | MovieFieldUpdated String
    | QuestionFieldUpdated String
    | MovieWasEntered
    | QuestionWasEntered


type alias Model =
    { tree : QuestionTree
    , currentNode : QuestionTree
    , state : State
    , movieFieldText : String
    , questionFieldText : String
    }


initialTree : QuestionTree
initialTree =
    Node "Does it star Arnold Schwarzenegger?" (Node "RoboCop" Empty Empty) (Node "The Terminator" Empty Empty)


init : Model
init =
    { tree = initialTree
    , currentNode = initialTree
    , state = Running
    , movieFieldText = ""
    , questionFieldText = ""
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        YesButtonPressed ->
            case model.state of
                Running ->
                    case model.currentNode of
                        Empty ->
                            { model | currentNode = Empty }

                        Node _ _ r ->
                            case r of
                                Empty ->
                                    { model | state = Won }

                                Node _ _ _ ->
                                    { model | currentNode = r }

                GotQuestion ->
                    let
                        newTree =
                            addNode model.questionFieldText model.movieFieldText model.currentNode Yes model.tree
                    in
                    { model
                        | state = Running
                        , tree = newTree
                        , currentNode = newTree
                    }

                _ ->
                    model

        NoButtonPressed ->
            case model.state of
                Running ->
                    case model.currentNode of
                        Empty ->
                            { model | currentNode = Empty }

                        Node _ l _ ->
                            case l of
                                Empty ->
                                    { model | state = Lost }

                                Node _ _ _ ->
                                    { model | currentNode = l }

                GotQuestion ->
                    let
                        newTree =
                            addNode model.questionFieldText model.movieFieldText model.currentNode No model.tree
                    in
                    { model
                        | state = Running
                        , tree = newTree
                        , currentNode = newTree
                    }

                _ ->
                    model

        MovieFieldUpdated t ->
            { model | movieFieldText = t }

        MovieWasEntered ->
            { model | state = GotMovie }

        QuestionFieldUpdated t ->
            { model | questionFieldText = t }

        QuestionWasEntered ->
            { model | state = GotQuestion }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )



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
                    column []
                        [ text "Bummer! I got it wrong."
                        , Input.text [ onEnter MovieWasEntered ]
                            { text = model.movieFieldText
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (text "What movie are you thinking of?")
                            , onChange = MovieFieldUpdated
                            }
                        ]

                GotMovie ->
                    column []
                        [ case model.currentNode of
                            Empty ->
                                text "This should not be possible!"

                            Node v _ _ ->
                                text <| "What question would distinguish " ++ model.movieFieldText ++ " from " ++ v ++ "?"
                        , Input.text [ onEnter QuestionWasEntered ]
                            { text = model.questionFieldText
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (text "")
                            , onChange = QuestionFieldUpdated
                            }
                        ]

                GotQuestion ->
                    column []
                        [ text <| "If I asked the question " ++ model.questionFieldText ++ " about " ++ model.movieFieldText ++ ", what would the answer be?"
                        , row [ centerX, spacing 50 ]
                            [ button [] { onPress = Just YesButtonPressed, label = text "Yes" }
                            , button [] { onPress = Just NoButtonPressed, label = text "No" }
                            ]
                        ]

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



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
