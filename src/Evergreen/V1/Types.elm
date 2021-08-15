module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.QuestionTree
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


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , tree : Evergreen.V1.QuestionTree.QuestionTree
    , currentNode : Evergreen.V1.QuestionTree.QuestionTree
    , state : GameState
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String
    }


type alias BackendModel =
    { tree : Evergreen.V1.QuestionTree.QuestionTree
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


type ToBackend
    = GetTree
    | AddMovie String String Evergreen.V1.QuestionTree.Answer Evergreen.V1.QuestionTree.QuestionTree


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TreeSent Evergreen.V1.QuestionTree.QuestionTree
