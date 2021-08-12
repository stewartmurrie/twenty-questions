module QuestionTree exposing (..)


type QuestionTree
    = Empty
    | Node String QuestionTree QuestionTree


type Answer
    = No
    | Yes


addNode : String -> String -> QuestionTree -> Answer -> QuestionTree -> QuestionTree
addNode question leaf node answer tree =
    case tree of
        Empty ->
            Empty

        Node value left right ->
            if node == tree then
                case answer of
                    No ->
                        Node question (Node leaf Empty Empty) node

                    Yes ->
                        Node question node (Node leaf Empty Empty)

            else
                Node value (addNode question leaf node answer left) (addNode question leaf node answer right)
