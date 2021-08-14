module Backend exposing (app)

import Array exposing (initialize)
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import QuestionTree exposing (Answer(..), QuestionTree(..))
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
    ( { tree = Node "Does it star Arnie Schwarzenegger?" (Node "RoboCop" Empty Empty) (Node "The Terminator" Empty Empty) }
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


addKnowledge : String -> String -> QuestionTree -> Answer -> QuestionTree -> QuestionTree
addKnowledge question movie node answer tree =
    -- TODO: consider replacing this recursive function with a fold
    case tree of
        Empty ->
            Empty

        Node value left right ->
            if node == tree then
                let
                    _ =
                        Debug.log "tree" tree

                    _ =
                        Debug.log "node" node
                in
                case answer of
                    No ->
                        Node question (Node movie Empty Empty) node

                    Yes ->
                        Node question node (Node movie Empty Empty)

            else
                Node value (addKnowledge question movie node answer left) (addKnowledge question movie node answer right)
