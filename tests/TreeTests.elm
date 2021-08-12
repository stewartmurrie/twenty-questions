module TreeTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import QuestionTree exposing (Answer(..), QuestionTree(..), addNode)
import Test exposing (..)


suite : Test
suite =
    describe "Testing trees" <|
        let
            singleMovie =
                Node "RoboCop" Empty Empty

            twoMovies =
                Node "is Arnie?"
                    (Node "RoboCop" Empty Empty)
                    (Node "The Terminator" Empty Empty)

            threeMovies =
                Node "is Arnie?"
                    (Node "RoboCop" Empty Empty)
                    (Node "is cyborg?"
                        (Node "Commando" Empty Empty)
                        (Node "The Terminator" Empty Empty)
                    )
        in
        [ test "Add second movie" <|
            \_ ->
                Expect.equal
                    twoMovies
                    (addNode "is Arnie?" "The Terminator" singleMovie Yes singleMovie)
        , test "Add third movie" <|
            \_ ->
                Expect.equal
                    threeMovies
                    (addNode "is cyborg?" "Commando" (Node "The Terminator" Empty Empty) No twoMovies)
        ]
