module Backend exposing (app)

import Array exposing (initialize)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import QuestionTree exposing (Answer(..), QuestionTree(..), addKnowledge)
import Types exposing (BackendModel, BackendMsg(..), ToBackend(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { tree = Node "Is it The Terminator?" (Node "RoboCop" Empty Empty) (Node "The Terminator" Empty Empty) }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GetTree ->
            ( model, sendToFrontend clientId (TreeSent model.tree) )

        AddMovie question movie answer node ->
            ( { model | tree = addKnowledge question movie node answer model.tree }, Cmd.none )
