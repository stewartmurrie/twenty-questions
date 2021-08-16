module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V2.QuestionTree
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
    , tree : Evergreen.V2.QuestionTree.QuestionTree
    , currentNode : Evergreen.V2.QuestionTree.QuestionTree
    , state : GameState
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String
    , movieCount : Int
    }


type alias BackendModel =
    { tree : Evergreen.V2.QuestionTree.QuestionTree
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
    | AddMovie String String Evergreen.V2.QuestionTree.Answer Evergreen.V2.QuestionTree.QuestionTree
    | GetMovieCount


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TreeSent Evergreen.V2.QuestionTree.QuestionTree
    | MovieCount Int
