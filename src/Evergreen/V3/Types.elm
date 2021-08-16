module Evergreen.V3.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V3.QuestionTree
import Url


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
    { key : Browser.Navigation.Key
    , tree : Evergreen.V3.QuestionTree.QuestionTree
    , currentNode : Evergreen.V3.QuestionTree.QuestionTree
    , state : GameState
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String
    , movieCount : Int
    , password : String
    }


type alias BackendModel =
    { tree : Evergreen.V3.QuestionTree.QuestionTree
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
    | UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PasswordFieldUpdated String


type ToBackend
    = GetTree
    | AddMovie String String Evergreen.V3.QuestionTree.Answer Evergreen.V3.QuestionTree.QuestionTree
    | GetMovieCount
    | ResetModel String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TreeSent Evergreen.V3.QuestionTree.QuestionTree
    | MovieCount Int
