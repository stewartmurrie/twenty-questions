module QuestionTree exposing (..)

-- TODO: consider using opaque types for this module
-- TODO: consider using a record type rather than this implicit binary structure.
-- This would give us proper yes/no fields (and possibly more in the future), and might make the
-- code a little more self-documenting.


type QuestionTree
    = Empty
    | Node String QuestionTree QuestionTree



-- TODO: hide this and create wrapper functions for adding new knowledge


type Answer
    = No
    | Yes


addKnowledge : String -> String -> QuestionTree -> Answer -> QuestionTree -> QuestionTree
addKnowledge question movie node answer tree =
    -- TODO: consider replacing this recursive function with a fold
    case tree of
        Empty ->
            Empty

        Node value left right ->
            if node == tree then
                case answer of
                    No ->
                        Node question (Node movie Empty Empty) node

                    Yes ->
                        Node question node (Node movie Empty Empty)

            else
                Node value (addKnowledge question movie node answer left) (addKnowledge question movie node answer right)
