module Main exposing (main)

import Browser
import Cmd.Extra
import Fields as F
import Html as H
import Select as WS
import Wizard
import Yafl as Y


main : Program () (Y.Model F.FormModel Output) Msg
main =
    Browser.document
        { init =
            \() ->
                init
                    { users =
                        [ { name = "Ed", id = 1 }
                        , { name = "Simon", id = 2 }
                        , { name = "Christian", id = 3 }
                        , { name = "Simon2", id = 2234 }
                        , { name = "Christian2", id = 3000 }
                        ]
                    , userId = 1
                    , projects =
                        [ { projectName = "yafl", id = 1 }
                        , { projectName = "safe-virtual-dom", id = 2 }
                        , { projectName = "elm-form", id = 3 }
                        , { projectName = "0yafl", id = 4 }
                        , { projectName = "1safe-virtual-dom", id = 5 }
                        , { projectName = "2elm-form", id = 6 }
                        , { projectName = "3yafl", id = 7 }
                        , { projectName = "4safe-virtual-dom", id = 8 }
                        , { projectName = "5elm-form", id = 9 }
                        ]
                    , projectId = 2
                    }
        , update = \msg model -> update msg model
        , view = \model -> { title = "", body = view model }
        , subscriptions = subscriptions
        }


type Msg
    = Updated (Y.Msg F.FormMsg)
    | Submitted


type alias Model =
    Y.Model F.FormModel Output


type alias Output =
    { userId : Int
    , projectId : Int
    }


type alias Input =
    { user : Maybe (WS.SelectMsg F.User)
    , project : Maybe (WS.SelectMsg F.Project)
    }


form : Y.Field F.FormModel F.FormMsg Never Never Input Output
form =
    Y.succeed
        (\project_ user_ ->
            { userId = user_.id
            , projectId = project_.id
            }
        )
        |> Y.andMap .project project
        |> Y.andMap .user user


project : Y.Field F.FormModel F.FormMsg Y.HasId (WS.SelectMsg F.Project) (WS.SelectMsg F.Project) F.Project
project =
    F.project


user : Y.Field F.FormModel F.FormMsg Y.HasId (WS.SelectMsg F.User) (WS.SelectMsg F.User) F.User
user =
    F.user


loader : Input
loader =
    { user = Nothing
    , project = Nothing
    }


init :
    { users : List F.User
    , userId : Int
    , projects : List F.Project
    , projectId : Int
    }
    -> ( Y.Model F.FormModel Output, Cmd Msg )
init { users, userId, projects, projectId } =
    Y.init form
        |> Cmd.Extra.andThen
            (Y.load form
                { loader
                    | user = Just (WS.Loaded { items = users, selected = Just userId })
                    , project = Just (WS.Loaded { items = projects, selected = Just projectId })
                }
            )
        |> Tuple.mapSecond (Cmd.map Updated)


update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Updated formMsg ->
            Y.update form formMsg model
                |> Tuple.mapSecond (Cmd.map Updated)

        Submitted ->
            ( model, Cmd.none )


view : Model -> List (H.Html Msg)
view model =
    Wizard.view
        Updated
        Submitted
        form
        model


subscriptions : Y.Model F.FormModel Output -> Sub Msg
subscriptions model =
    Y.subscriptions form model
        |> Sub.map Updated
