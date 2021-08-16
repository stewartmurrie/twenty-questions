module Evergreen.Migrate.V2 exposing (..)

import Evergreen.V1.QuestionTree as V1 exposing (Answer(..), QuestionTree)
import Evergreen.V1.Types as Old
import Evergreen.V2.QuestionTree as V2 exposing (Answer(..), QuestionTree(..))
import Evergreen.V2.Types as New
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


upgradeTree : V1.QuestionTree -> V2.QuestionTree
upgradeTree old =
    case old of
        V1.Empty ->
            V2.Empty

        V1.Node v l r ->
            V2.Node v (upgradeTree l) (upgradeTree r)


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


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    case old of
        Old.GetTree ->
            MsgMigrated ( New.GetTree, Cmd.none )

        Old.AddMovie a b c d ->
            MsgMigrated ( New.AddMovie a b (upgradeAnswer c) (upgradeTree d), Cmd.none )


upgradeAnswer : V1.Answer -> V2.Answer
upgradeAnswer old =
    case old of
        V1.No ->
            V2.No

        V1.Yes ->
            V2.Yes


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    case old of
        Old.TreeSent tree ->
            MsgMigrated ( New.TreeSent (upgradeTree tree), Cmd.none )
