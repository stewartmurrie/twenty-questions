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
