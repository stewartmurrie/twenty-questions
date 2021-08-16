module Evergreen.V2.QuestionTree exposing (..)


type QuestionTree
    = Empty
    | Node String QuestionTree QuestionTree


type Answer
    = No
    | Yes
