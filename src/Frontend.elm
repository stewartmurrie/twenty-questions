module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Color, Element, alignBottom, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, rgb, rgb255, row, spacing, spacingXY, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Events
import Json.Decode as Decode
import Lamdera
import QuestionTree exposing (Answer(..), QuestionTree(..))
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
        , subscriptions = \_ -> Sub.none
        , view = view
        }


initialTree : QuestionTree
initialTree =
    Empty


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , tree = initialTree
      , currentNode = initialTree
      , state = InLobby
      , movieFieldText = ""
      , questionFieldText = ""
      , questionLog = []
      }
    , Cmd.none
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
                    -- The player entered a movie and a question and confirmed it was right.
                    -- Add the movie to the database.
                    ( { model
                        | state = MovieAdded
                      }
                    , Lamdera.sendToBackend (AddMovie model.questionFieldText model.movieFieldText Yes model.currentNode)
                    )

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
                    -- The player entered a movie and a question, but didn't like it.
                    -- Allow them to edit the question.
                    noCmd
                        { model
                            | state = EditQuestion
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

        PlayButtonPressed ->
            ( { model
                | state = Running
                , currentNode = model.tree
                , questionFieldText = ""
                , movieFieldText = ""
                , questionLog = []
              }
            , Lamdera.sendToBackend GetTree
            )

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
                  -- TODO: this reset appears under PlayButtonPressed above. Factor it out.
                    | state = Running
                    , tree = tree
                    , currentNode = tree
                    , questionFieldText = ""
                    , movieFieldText = ""
                    , questionLog = []
                }



-- VIEW


neonGreen : Color
neonGreen =
    rgb255 77 209 0


neonPink : Color
neonPink =
    rgb255 248 53 218


neonBlue : Color
neonBlue =
    rgb255 0 162 209


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Movie Qs!"
    , body =
        [ Element.layout [ Background.color (rgb255 220 220 220) ] <|
            column [ centerX, width (375 |> px), height fill, padding 40, Background.color (rgb255 12 11 11), Font.color (rgb255 245 245 245) ]
                [ column [ centerX, paddingXY 0 48, spacing 8 ] <|
                    [ el [ centerX, Font.size 48, Font.bold ] (text "Movie Q's")
                    , el [ centerX ] (text "The Movie Guessing Game")
                    ]
                , case model.state of
                    InLobby ->
                        column [ width fill, spacing 30, Font.size 17 ]
                            [ text "Think of a movie."
                            , paragraph []
                                [ text "I'll try to guess it by asking questions." ]
                            , paragraph [ spacing 8 ]
                                [ text "You answer with "
                                , el [ Font.bold, Font.italic, Font.color neonGreen ] (text "YES")
                                , text " or "
                                , el [ Font.bold, Font.italic, Font.color neonBlue ] (text "NO")
                                , text "."
                                ]
                            , paragraph [ spacing 8 ] [ text "If I'm wrong, tell me your movie and I'll remember it for next time." ]
                            , paragraph [] [ text "I know ", el [ Font.bold, Font.color neonPink ] (text "23"), text " movies." ]
                            , text "Ready to play?"
                            , el [ width fill, paddingXY 0 32 ] (primaryButton neonPink "Let's Play!" PlayButtonPressed)
                            ]

                    Won ->
                        column [ centerX ]
                            [ viewQuestionLog model.questionLog
                            , el [] (text <| "Yay! I guessed right!")
                            , primaryButton neonPink "Play Again" PlayButtonPressed
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
                                                        "What question about "
                                                    , el [ Font.semiBold, Font.italic ]
                                                        (text <|
                                                            model.movieFieldText
                                                        )
                                                    , text <|
                                                        " could I ask to distinguish it from "
                                                    , el [ Font.semiBold, Font.italic ] (text v)
                                                    , text "? (It should be one that can be answered with YES)"
                                                    ]
                                                )
                                        , onChange = QuestionFieldUpdated
                                        }
                            ]

                    GotQuestion ->
                        column []
                            [ paragraph []
                                [ text <| "Let me make sure I understand."
                                , text <| "For the movie "
                                , el [ Font.italic, Font.semiBold ] (text model.movieFieldText)
                                , text <| " the answer to the question "
                                , el [ Font.italic ] (text <| "'" ++ model.questionFieldText ++ "'")
                                , text <| " is YES. Correct?"
                                ]
                            , row [ centerX, width fill, spacing 50, paddingXY 0 20 ]
                                [ primaryButton neonGreen "Yes" YesButtonPressed
                                , primaryButton neonBlue "No" NoButtonPressed
                                ]
                            ]

                    EditQuestion ->
                        -- TODO: there's a lot here shared with GotMovie above. Factor it out.
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
                                                        "Ok. Try asking a different question. What question about "
                                                    , el [ Font.semiBold, Font.italic ]
                                                        (text <|
                                                            model.movieFieldText
                                                        )
                                                    , text <|
                                                        " could I ask to distinguish it from "
                                                    , el [ Font.semiBold, Font.italic ] (text v)
                                                    , text "? (It should be one that can be answered with YES)"
                                                    ]
                                                )
                                        , onChange = QuestionFieldUpdated
                                        }
                            ]

                    MovieAdded ->
                        column [ centerX, width fill, spacing 20 ]
                            [ paragraph []
                                [ text <| "Ok! I'll remember "
                                , el [ Font.italic, Font.semiBold ] (text model.movieFieldText)
                                , text " for next time."
                                ]
                            , primaryButton neonPink "Play Again" PlayButtonPressed
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
                                [ primaryButton neonGreen "Yes" YesButtonPressed
                                , primaryButton neonBlue "No" NoButtonPressed
                                ]
                            ]
                , row [ centerX, alignBottom, Font.size 13, Font.color (rgb255 150 150 150) ]
                    [ el [] (text "@stewartmurrie")
                    , el [] (text " | ")
                    , el [] (text "made with elm")
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


primaryButton : Color -> String -> FrontendMsg -> Element FrontendMsg
primaryButton color label msg =
    button
        [ Background.color color
        , Border.solid
        , Border.width 2
        , Border.color (rgb255 0 0 0)
        , Border.glow color 4
        , rounded 32
        , paddingXY 16 0
        , width fill
        , height (56 |> px)
        ]
        { onPress = Just msg
        , label = el [ centerX, Font.bold, Font.size 21, Font.letterSpacing 0.37 ] (text (String.toUpper label))
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
