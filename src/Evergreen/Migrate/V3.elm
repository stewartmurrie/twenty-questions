module Evergreen.Migrate.V3 exposing (..)

import Evergreen.V2.QuestionTree as V2 exposing (Answer(..), QuestionTree)
import Evergreen.V2.Types as Old
import Evergreen.V3.QuestionTree as V3 exposing (Answer(..), QuestionTree(..))
import Evergreen.V3.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key
          , tree = upgradeTree old.tree
          , currentNode = upgradeTree old.currentNode
          , state = upgradeGameState old.state
          , movieFieldText = old.movieFieldText
          , questionFieldText = old.questionFieldText
          , questionLog = old.questionLog
          , movieCount = 0
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged


upgradeAnswer : V2.Answer -> V3.Answer
upgradeAnswer old =
    case old of
        V2.No ->
            V3.No

        V2.Yes ->
            V3.Yes


upgradeTree : V2.QuestionTree -> V3.QuestionTree
upgradeTree old =
    case old of
        V2.Empty ->
            V3.Empty

        V2.Node v l r ->
            V3.Node v (upgradeTree l) (upgradeTree r)


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    case old of
        Old.GetTree ->
            MsgMigrated ( New.GetTree, Cmd.none )

        Old.AddMovie a b c d ->
            MsgMigrated ( New.AddMovie a b (upgradeAnswer c) (upgradeTree d), Cmd.none )

        Old.GetMovieCount ->
            MsgMigrated ( New.GetMovieCount, Cmd.none )


upgradeGameState : Old.GameState -> New.GameState
upgradeGameState old =
    case old of
        Old.InLobby ->
            New.InLobby

        Old.Running ->
            New.Running

        Old.Won ->
            New.Won

        Old.Lost ->
            New.Lost

        Old.GotMovie ->
            New.GotMovie

        Old.GotQuestion ->
            New.GotQuestion

        Old.MovieAdded ->
            New.MovieAdded

        Old.EditQuestion ->
            New.EditQuestion
