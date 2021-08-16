module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import QuestionTree exposing (Answer, QuestionTree)
import Url exposing (Url)


type GameState
    = InLobby
    | Running
    | Won
    | Lost
    | GotMovie
    | GotQuestion
    | MovieAdded
    | EditQuestion
    | ConfirmModelReset


type alias FrontendModel =
    { key : Key
    , tree : QuestionTree
    , currentNode : QuestionTree
    , state : GameState
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String -- TODO: consider replacing this with a list of nodes, or path through the tree
    , movieCount : Int
    , password : String
    }


type alias BackendModel =
    { tree : QuestionTree
    }


type FrontendMsg
    = NoOpFrontendMsg
    | YesButtonPressed
    | NoButtonPressed
    | MovieFieldUpdated String
    | QuestionFieldUpdated String
    | MovieWasEntered
    | QuestionWasEntered
    | PlayButtonPressed
    | UrlClicked UrlRequest
    | UrlChanged Url
    | PasswordFieldUpdated String


type ToBackend
    = GetTree
    | AddMovie String String Answer QuestionTree
    | GetMovieCount
    | ResetModel String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TreeSent QuestionTree
    | MovieCount Int
