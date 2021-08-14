module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import QuestionTree exposing (QuestionTree)
import Url exposing (Url)


type GameState
    = Running
    | Won
    | Lost
    | GotMovie
    | GotQuestion
    | MovieAdded


type alias FrontendModel =
    { key : Key
    , tree : QuestionTree
    , currentNode : QuestionTree
    , state : GameState
    , movieFieldText : String
    , questionFieldText : String
    , questionLog : List String -- TODO: consider replacing this with a list of nodes, or path through the tree
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | YesButtonPressed
    | NoButtonPressed
    | MovieFieldUpdated String
    | QuestionFieldUpdated String
    | MovieWasEntered
    | QuestionWasEntered
    | PlayAgainButtonPressed
    | UrlClicked UrlRequest
    | UrlChanged Url


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend