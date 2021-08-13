module Main exposing (main)

import Browser
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, rgb, rgb255, row, spacing, spacingXY, text, textColumn, width)
import Element.Background exposing (color)
import Element.Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import QuestionTree exposing (Answer(..), QuestionTree(..), addKnowledge)



-- MODEL
-- 20Q I think is represented by a binary tree. Each node is a question, with YES or NO branches.
-- Leaf nodes represent answers?


type State
    = Running
    | Won
    | Lost
    | GotMovie
    | GotQuestion
    | MovieAdded


type Msg
    = YesButtonPressed
    | NoButtonPressed
    | MovieFieldUpdated String
    | QuestionFieldUpdated String
    | MovieWasEntered
    | QuestionWasEntered
    | PlayAgainButtonPressed


type alias Model =
    { tree : QuestionTree
    , currentNode : QuestionTree
    , state : State
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String -- TODO: consider replacing this with a list of nodes, or path through the tree
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
    , questionLog = []
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
                            -- Should be impossible!
                            { model | currentNode = Empty }

                        Node question _ right ->
                            case right of
                                Empty ->
                                    -- Our question was a leaf node, and we guessed right!
                                    { model
                                        | state = Won
                                        , questionLog = ("Is it the movie " ++ question ++ "? YES") :: model.questionLog
                                    }

                                Node _ _ _ ->
                                    -- Our question was a regular node, so traverse to the right
                                    { model
                                        | currentNode = right
                                        , questionLog = (question ++ " YES") :: model.questionLog
                                    }

                GotQuestion ->
                    let
                        newTree =
                            addKnowledge model.questionFieldText model.movieFieldText model.currentNode Yes model.tree
                    in
                    { model
                        | state = MovieAdded
                        , tree = newTree
                    }

                _ ->
                    model

        NoButtonPressed ->
            case model.state of
                Running ->
                    case model.currentNode of
                        Empty ->
                            -- Should be impossible!
                            { model | currentNode = Empty }

                        Node question left _ ->
                            case left of
                                Empty ->
                                    -- Our question was a leaf node, and our guess was wrong :(
                                    { model
                                        | state = Lost
                                        , questionLog = ("Is it the movie " ++ question ++ "? NO") :: model.questionLog
                                    }

                                Node _ _ _ ->
                                    -- Our question was a regular node, so traverse left
                                    { model
                                        | currentNode = left
                                        , questionLog = (question ++ " NO") :: model.questionLog
                                    }

                GotQuestion ->
                    let
                        newTree =
                            addKnowledge model.questionFieldText model.movieFieldText model.currentNode No model.tree
                    in
                    { model
                        | state = MovieAdded
                        , tree = newTree
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
            let
                question =
                    if String.endsWith "?" model.questionFieldText then
                        model.questionFieldText

                    else
                        model.questionFieldText ++ "?"
            in
            { model | state = GotQuestion, questionFieldText = question }

        PlayAgainButtonPressed ->
            { model
                | state = Running
                , currentNode = model.tree
                , questionFieldText = ""
                , movieFieldText = ""
                , questionLog = []
            }


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
    Element.layout [ centerX, color (rgb255 220 220 220), padding 20 ] <|
        column
            [ centerX ]
            [ el [ centerX ] <|
                el
                    [ Font.size 48
                    , Font.bold
                    , paddingXY 0 50
                    ]
                    (text "20 Movie Questions")
            , case model.state of
                Won ->
                    column [ centerX, width fill, spacing 20 ]
                        [ viewQuestionLog model.questionLog
                        , el [] (text <| "Yay! I guessed right!")
                        , primaryButton "Play Again" PlayAgainButtonPressed
                        ]

                Lost ->
                    column [ width fill, spacing 20 ]
                        [ viewQuestionLog model.questionLog
                        , text "Bummer! I got it wrong."
                        , Input.text [ onEnter MovieWasEntered, spacing 10 ]
                            { text = model.movieFieldText
                            , placeholder = Nothing
                            , label = Input.labelAbove [] (text "What movie were you thinking of?")
                            , onChange = MovieFieldUpdated
                            }
                        ]

                GotMovie ->
                    column [ width fill, spacing 20 ]
                        [ case model.currentNode of
                            Empty ->
                                -- Should be impossible!
                                text "This should not be possible!"

                            Node v _ _ ->
                                Input.text [ onEnter QuestionWasEntered, spacing 10 ]
                                    { text = model.questionFieldText
                                    , placeholder = Nothing
                                    , label =
                                        Input.labelAbove [ spacing 20 ]
                                            (paragraph [ centerX, width fill ]
                                                [ text <|
                                                    "What question would distinguish "
                                                , el [ Font.semiBold, Font.italic ]
                                                    (text <|
                                                        model.movieFieldText
                                                    )
                                                , text <|
                                                    " from "
                                                , el [ Font.semiBold, Font.italic ] (text v)
                                                , text "?"
                                                ]
                                            )
                                    , onChange = QuestionFieldUpdated
                                    }
                        ]

                GotQuestion ->
                    column []
                        [ paragraph []
                            [ text <| "If I asked the question "
                            , el [ Font.italic ] (text <| "'" ++ model.questionFieldText ++ "'")
                            , text <| " about "
                            , el [ Font.italic, Font.semiBold ] (text model.movieFieldText)
                            , text <|
                                ", what would the answer be?"
                            ]
                        , row [ centerX, width fill, spacing 50, paddingXY 0 20 ]
                            [ primaryButton "Yes" YesButtonPressed
                            , primaryButton "No" NoButtonPressed
                            ]
                        ]

                MovieAdded ->
                    column [ centerX, width fill, spacing 20 ]
                        [ paragraph []
                            [ text <| "Ok! I'll remember "
                            , el [ Font.italic, Font.semiBold ] (text model.movieFieldText)
                            , text " for next time."
                            ]
                        , primaryButton "Play Again" PlayAgainButtonPressed
                        ]

                Running ->
                    column [ centerX, width fill ]
                        [ viewQuestionLog model.questionLog
                        , case model.currentNode of
                            Empty ->
                                -- Should be impossible
                                text "This shouldn't happen!"

                            Node n l _ ->
                                case l of
                                    Empty ->
                                        paragraph []
                                            [ text <| "Q" ++ viewQuestionCount model.questionLog ++ ": "
                                            , text <| "Is it the movie "
                                            , el [ Font.semiBold, Font.italic ] (text n)
                                            , text "?"
                                            ]

                                    _ ->
                                        paragraph []
                                            [ text <| "Q" ++ viewQuestionCount model.questionLog ++ ": "
                                            , text n
                                            ]
                        , row [ centerX, width fill, spacing 50, paddingXY 0 20 ]
                            [ primaryButton "Yes" YesButtonPressed
                            , primaryButton "No" NoButtonPressed
                            ]
                        ]
            ]


viewQuestionLog : List String -> Element Msg
viewQuestionLog log =
    column [ spacing 10 ]
        (log
            |> List.reverse
            |> List.indexedMap (\i q -> el [] <| text <| "Q" ++ String.fromInt (i + 1) ++ ": " ++ q)
        )


viewQuestionCount : List String -> String
viewQuestionCount log =
    log |> List.length |> (+) 1 |> String.fromInt


primaryButton : String -> Msg -> Element Msg
primaryButton label msg =
    button
        [ paddingXY 30 15
        , color (rgb255 176 67 138)
        , rounded 5
        , Font.color (rgb 1 1 1)
        , width fill
        , centerX
        , centerY
        , height (80 |> px)
        ]
        { onPress = Just msg
        , label = el [ centerX, Font.bold, Font.size 24 ] (text label)
        }



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
