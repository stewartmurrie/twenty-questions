module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, rgb, rgb255, row, spacing, spacingXY, text, textColumn, width)
import Element.Background exposing (color)
import Element.Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Lamdera
import QuestionTree exposing (Answer(..), QuestionTree(..), addKnowledge)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


initialTree : QuestionTree
initialTree =
    Node "Does it star Arnold Schwarzenegger?" (Node "RoboCop" Empty Empty) (Node "The Terminator" Empty Empty)


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , tree = initialTree
      , currentNode = initialTree
      , state = Running
      , movieFieldText = ""
      , questionFieldText = ""
      , questionLog = []
      }
    , Lamdera.sendToBackend GetTree
    )


noCmd : Model -> ( Model, Cmd FrontendMsg )
noCmd model =
    ( model, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            noCmd model

        YesButtonPressed ->
            case model.state of
                Running ->
                    case model.currentNode of
                        Empty ->
                            -- Should be impossible!
                            noCmd { model | currentNode = Empty }

                        Node question _ right ->
                            case right of
                                Empty ->
                                    -- Our question was a leaf node, and we guessed right!
                                    noCmd
                                        { model
                                            | state = Won
                                            , questionLog = ("Is it the movie " ++ question ++ "? YES") :: model.questionLog
                                        }

                                Node _ _ _ ->
                                    -- Our question was a regular node, so traverse to the right
                                    noCmd
                                        { model
                                            | currentNode = right
                                            , questionLog = (question ++ " YES") :: model.questionLog
                                        }

                GotQuestion ->
                    let
                        newTree =
                            addKnowledge model.questionFieldText model.movieFieldText model.currentNode Yes model.tree
                    in
                    noCmd
                        { model
                            | state = MovieAdded
                            , tree = newTree
                        }

                _ ->
                    noCmd model

        NoButtonPressed ->
            case model.state of
                Running ->
                    case model.currentNode of
                        Empty ->
                            -- Should be impossible!
                            noCmd { model | currentNode = Empty }

                        Node question left _ ->
                            case left of
                                Empty ->
                                    -- Our question was a leaf node, and our guess was wrong :(
                                    noCmd
                                        { model
                                            | state = Lost
                                            , questionLog = ("Is it the movie " ++ question ++ "? NO") :: model.questionLog
                                        }

                                Node _ _ _ ->
                                    -- Our question was a regular node, so traverse left
                                    noCmd
                                        { model
                                            | currentNode = left
                                            , questionLog = (question ++ " NO") :: model.questionLog
                                        }

                GotQuestion ->
                    let
                        newTree =
                            addKnowledge model.questionFieldText model.movieFieldText model.currentNode No model.tree
                    in
                    noCmd
                        { model
                            | state = MovieAdded
                            , tree = newTree
                        }

                _ ->
                    noCmd model

        MovieFieldUpdated t ->
            noCmd { model | movieFieldText = t }

        MovieWasEntered ->
            noCmd { model | state = GotMovie }

        QuestionFieldUpdated t ->
            noCmd { model | questionFieldText = t }

        QuestionWasEntered ->
            let
                question =
                    if String.endsWith "?" model.questionFieldText then
                        model.questionFieldText

                    else
                        model.questionFieldText ++ "?"
            in
            noCmd { model | state = GotQuestion, questionFieldText = question }

        PlayAgainButtonPressed ->
            noCmd
                { model
                    | state = Running
                    , currentNode = model.tree
                    , questionFieldText = ""
                    , movieFieldText = ""
                    , questionLog = []
                }

        UrlChanged _ ->
            noCmd model

        UrlClicked _ ->
            noCmd model


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TreeSent tree ->
            noCmd
                { model
                    | state = Running
                    , tree = tree
                    , currentNode = tree
                    , questionFieldText = ""
                    , movieFieldText = ""
                    , questionLog = []
                }


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Movie Qs!"
    , body =
        [ Element.layout [ centerX, color (rgb255 220 220 220), padding 20 ] <|
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
        ]
    }


viewQuestionLog : List String -> Element FrontendMsg
viewQuestionLog log =
    column [ spacing 10 ]
        (log
            |> List.reverse
            |> List.indexedMap (\i q -> el [] <| text <| "Q" ++ String.fromInt (i + 1) ++ ": " ++ q)
        )


viewQuestionCount : List String -> String
viewQuestionCount log =
    log |> List.length |> (+) 1 |> String.fromInt


primaryButton : String -> FrontendMsg -> Element FrontendMsg
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
