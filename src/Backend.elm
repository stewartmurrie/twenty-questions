module Backend exposing (app)

import Env
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
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
    let
        initModel =
            { tree = Node "Is it a sci-fi movie?" (Node "Titanic" Empty Empty) (Node "Star Wars: Episode IV—A New Hope" Empty Empty) }
    in
    ( initModel, broadcast (MovieCount (QuestionTree.countAnswers initModel.tree)) )


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
            let
                newTree =
                    addKnowledge question movie node answer model.tree
            in
            ( { model | tree = newTree }, broadcast (MovieCount (QuestionTree.countAnswers newTree)) )

        GetMovieCount ->
            ( model, sendToFrontend clientId (MovieCount (QuestionTree.countAnswers model.tree)) )

        ResetModel password ->
            if password == Env.resetPassword then
                init

            else
                ( model, Cmd.none )
