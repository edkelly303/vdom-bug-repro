module Fields exposing
    ( FormModel
    , FormMsg
    , Project
    , User
    , fields
    , project
    , user
    )

import Html as H
import Select as WS
import Yafl as Y


type alias FormMsg =
    ( Maybe (WS.SelectMsg User)
    , ( Maybe (WS.SelectMsg Project)
      , ()
      )
    )


type alias FormModel =
    ( Maybe (WS.SelectModel User)
    , ( Maybe (WS.SelectModel Project)
      , ()
      )
    )


fields =
    Y.defineFields
        (\selectUser_ selectProject_ ->
            { selectUser = selectUser_
            , selectProject = selectProject_
            }
        )
        |> Y.addWidgetWithConfig WS.widget
        |> Y.addWidgetWithConfig WS.widget
        |> Y.endFields


type alias User =
    { id : Int
    , name : String
    }


type alias Project =
    { id : Int, projectName : String }


viewUser : User -> H.Html msg
viewUser str =
    H.text str.name


viewProject : Project -> H.Html msg
viewProject str =
    H.text str.projectName


user : Y.Field FormModel FormMsg Y.HasId (WS.SelectMsg User) (WS.SelectMsg User) User
user =
    fields.selectUser
        { showFilter = True
        , id = "user-select"
        , toInt = .id
        , toFilterString = .name
        , toHtml = viewUser
        }
        |> Y.identifier "user"
        |> Y.label "Which user did the majority of the work?"


project : Y.Field FormModel FormMsg Y.HasId (WS.SelectMsg Project) (WS.SelectMsg Project) Project
project =
    fields.selectProject
        { showFilter = True
        , id = "project-select"
        , toInt = .id
        , toFilterString = .projectName
        , toHtml = viewProject
        }
        |> Y.identifier "project"
        |> Y.label "Which project is the allocation for?"
