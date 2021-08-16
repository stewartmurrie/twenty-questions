module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Color, Element, alignBottom, centerX, centerY, column, el, fill, height, maximum, minimum, padding, paddingEach, paddingXY, paragraph, px, rgb, rgb255, row, spacing, spacingXY, text, textColumn, width)
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
                            | state = GotMovie
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
                rawQuestion =
                    String.trim model.questionFieldText

                question =
                    if String.endsWith "?" rawQuestion then
                        rawQuestion

                    else
                        rawQuestion ++ "?"
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
-- COLOR CONSTANTS


neonGreen : Color
neonGreen =
    rgb255 77 209 0


neonPink : Color
neonPink =
    rgb255 248 53 218


neonBlue : Color
neonBlue =
    rgb255 0 162 209


black : Color
black =
    rgb255 15 15 15


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Movie Qs!"
    , body =
        [ Element.layout [ Background.color black ] <|
            column [ centerX, width (fill |> minimum 375 |> maximum 600), height fill, padding 24, Background.color black, Font.size 17, Font.color (rgb255 245 245 245) ]
                -- Header
                [ column [ centerX, paddingEach { top = 16, left = 0, right = 0, bottom = 56 }, spacing 8 ] <|
                    [ el [ centerX, Font.size 48, Font.bold ] (text "Movie Q's")
                    , el [ centerX ] (text "The Movie Guessing Game")
                    ]
                , case model.state of
                    InLobby ->
                        column [ width fill, spacing 30 ]
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

                    Running ->
                        column [ width fill, spacing 20 ]
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
                                                , text <| "Is it "
                                                , el [ Font.semiBold, Font.italic, Font.color neonPink ] (text n)
                                                , el [ Font.italic ] (text "?")
                                                ]

                                        _ ->
                                            paragraph []
                                                [ text <| "Q" ++ viewQuestionCount model.questionLog ++ ": "
                                                , text n
                                                ]
                            , row [ centerX, width fill, spacing 24, paddingXY 0 48 ]
                                [ primaryButton neonGreen "Yes" YesButtonPressed
                                , primaryButton neonBlue "No" NoButtonPressed
                                ]
                            ]

                    Won ->
                        column [ centerX, spacing 20 ]
                            [ viewQuestionLog model.questionLog
                            , el [] (text <| "Yay! I guessed right!")
                            , el [ paddingXY 0 48, width fill ] (primaryButton neonPink "Play Again" PlayButtonPressed)
                            ]

                    Lost ->
                        column [ width fill, spacing 20 ]
                            [ viewQuestionLog model.questionLog
                            , text "Bummer! I guessed wrong."
                            , Input.multiline [ onEnter MovieWasEntered, Background.color black, Border.color neonPink, Border.glow neonPink 1.5 ]
                                { text = model.movieFieldText
                                , placeholder = Nothing
                                , label = Input.labelAbove [ paddingXY 0 16 ] (text "What movie were you thinking of?")
                                , onChange = MovieFieldUpdated
                                , spellcheck = False
                                }
                            , el [ paddingXY 0 48, width fill ] (primaryButton neonPink "Next" MovieWasEntered)
                            ]

                    GotMovie ->
                        column [ width fill, spacing 20 ]
                            [ case model.currentNode of
                                Empty ->
                                    -- Should be impossible!
                                    text "This should not be possible!"

                                Node v _ _ ->
                                    Input.multiline [ onEnter QuestionWasEntered, Background.color black, Border.color neonPink, Border.glow neonPink 1.5 ]
                                        { text = model.questionFieldText
                                        , placeholder = Nothing
                                        , label =
                                            Input.labelAbove []
                                                (column [ paddingXY 0 24, spacing 20 ]
                                                    [ paragraph [ spacing 8 ]
                                                        [ text "What question about "
                                                        , el [ Font.semiBold, Font.italic, Font.color neonPink ]
                                                            (text model.movieFieldText)
                                                        , text " could I ask to distinguish it from "
                                                        , el [ Font.semiBold, Font.italic, Font.color neonPink ] (text v)
                                                        , el [ Font.italic ] (text "?")
                                                        ]
                                                    , paragraph [ spacing 8 ]
                                                        [ el [ Font.italic ] (text "(It should be one that can be answered with ")
                                                        , el [ Font.italic, Font.bold, Font.color neonGreen ] (text "YES")
                                                        , el [ Font.italic ] (text ")")
                                                        ]
                                                    ]
                                                )
                                        , onChange = QuestionFieldUpdated
                                        , spellcheck = False
                                        }
                            , el [ paddingXY 0 48, width fill ] (primaryButton neonPink "Next" QuestionWasEntered)
                            ]

                    GotQuestion ->
                        column [ spacing 20, width fill ]
                            [ text <| "Let's make sure I understand."
                            , paragraph []
                                [ text <| "For the movie "
                                , el [ Font.italic, Font.bold, Font.color neonPink ] (text model.movieFieldText)
                                , text <| ", if I ask the question: "
                                ]
                            , paragraph [] [ el [ Font.italic ] (text <| "'" ++ model.questionFieldText ++ "'") ]
                            , paragraph []
                                [ text <| "the answer is "
                                , el [ Font.bold, Font.italic, Font.color neonGreen ] (text "YES")
                                , text ". Correct?"
                                ]
                            , row [ centerX, width fill, spacing 24, paddingXY 0 48 ]
                                [ primaryButton neonGreen "Yes" YesButtonPressed
                                , primaryButton neonBlue "No" NoButtonPressed
                                ]
                            ]

                    EditQuestion ->
                        -- TODO: probably remove this, since I just use the existing question entry stuff above
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
                                , el [ Font.italic, Font.bold, Font.color neonPink ] (text model.movieFieldText)
                                , text " for next time."
                                ]
                            , el [ width fill, paddingXY 0 48 ] (primaryButton neonPink "Play Again" PlayButtonPressed)
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
    column [ spacing 20 ]
        (log
            |> List.reverse
            |> List.indexedMap
                (\i q ->
                    paragraph [] [ text <| "Q" ++ String.fromInt (i + 1) ++ ": " ++ q ]
                )
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
