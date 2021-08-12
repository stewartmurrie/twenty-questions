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
addKnowledge question answer node branch tree =
    -- TODO: consider replacing this recursive function with a fold
    case tree of
        Empty ->
            Empty

        Node value left right ->
            if node == tree then
                case branch of
                    No ->
                        Node question (Node answer Empty Empty) node

                    Yes ->
                        Node question node (Node answer Empty Empty)

            else
                Node value (addKnowledge question answer node branch left) (addKnowledge question answer node branch right)
