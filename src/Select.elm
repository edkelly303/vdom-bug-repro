module Select exposing (SelectConfig, SelectModel, SelectMsg(..), widget)

import Helpers
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Task
import Yafl as Y


type alias SelectModel item =
    { selected : Maybe Int
    , filter : String
    , items : List item
    , id : String
    }


type SelectMsg item
    = Selected (Maybe Int)
    | Clear
    | Filtered String
    | Loaded { items : List item, selected : Maybe Int }
    | ScrollResultReceived


type alias SelectConfig item =
    { showFilter : Bool
    , id : String
    , toInt : item -> Int
    , toFilterString : item -> String
    , toHtml : item -> H.Html (SelectMsg item)
    }


widget : Y.Widget (SelectConfig item) (SelectModel item) (SelectMsg item) item
widget config =
    { view = view config
    , init = init config
    , update = update
    , submit = submit config
    , subscriptions = \_ -> Sub.none
    , label = "Select"
    }


init : SelectConfig item -> ( SelectModel item, Cmd (SelectMsg item) )
init config =
    ( { selected = Nothing
      , filter = ""
      , items = []
      , id = config.id
      }
    , Cmd.none
    )


update : SelectMsg item -> SelectModel item -> ( SelectModel item, Cmd (SelectMsg item) )
update =
    \msg model ->
        case msg of
            Clear ->
                ( { model | items = [], selected = Nothing }
                , Cmd.none
                )

            Loaded { items, selected } ->
                let
                    ( newSelected, cmd ) =
                        case selected of
                            Just selectedId ->
                                ( selected
                                , Helpers.scrollElementToMiddle
                                    { containerId = "yafl-select-list-" ++ model.id
                                    , elementId = "yafl-select-item-" ++ model.id ++ "-" ++ String.fromInt selectedId
                                    }
                                    |> Task.attempt (\_ -> ScrollResultReceived)
                                )

                            Nothing ->
                                ( model.selected
                                , Cmd.none
                                )
                in
                ( { model | items = items, selected = newSelected }
                , cmd
                )

            Filtered filter ->
                ( { model
                    | filter = filter
                  }
                , Cmd.none
                )

            Selected selection ->
                ( { model | selected = selection }
                , Cmd.none
                )

            ScrollResultReceived ->
                ( model, Cmd.none )


submit : SelectConfig item -> SelectModel item -> Result (List String) item
submit config model =
    case model.selected of
        Nothing ->
            Err [ "You must select an option" ]

        Just selection ->
            List.foldl
                (\item output ->
                    case output of
                        Ok _ ->
                            output

                        Err e ->
                            if config.toInt item == selection then
                                Ok item

                            else
                                Err e
                )
                (Err [ "The selected item is not one of the options" ])
                model.items


view :
    SelectConfig item
    -> { feedback : List Y.Feedback, label : String, id : String }
    -> SelectModel item
    -> List (H.Html (SelectMsg item))
view config { label } model =
    viewFilter label config model
        ++ viewItems config model label



-- ++ [ Form.Widgets.viewFeedback feedback ]


viewFilter : String -> SelectConfig item -> SelectModel item -> List (H.Html (SelectMsg item))
viewFilter label config model =
    [ H.label [] [ H.text label ]
    , if config.showFilter then
        H.span [ HA.class "yafl-filter" ]
            [ H.input
                [ HA.type_ "search"
                , HA.id ("yafl-filter-" ++ label)
                , HA.placeholder "Type to filter"
                , HE.onInput Filtered

                -- , Helpers.onKeyUp "Escape" (JD.succeed (Filtered ""))
                , HA.value model.filter
                ]
                []
            , H.button
                [ HA.type_ "button"
                , HE.onClick (Filtered "")
                ]
                [ H.text "✖️" ]
            ]

      else
        H.text ""
    ]


viewItems : SelectConfig item -> SelectModel item -> String -> List (H.Html (SelectMsg item))
viewItems config model label =
    case model.items of
        [] ->
            [ H.div [ HA.class "yafl-select-no-options" ] [ H.text "⚠️ No options available" ] ]

        items ->
            let
                filtered =
                    if String.isEmpty model.filter then
                        items

                    else
                        List.filter
                            (\item ->
                                let
                                    haystack =
                                        config.toFilterString item
                                in
                                String.contains
                                    (String.toLower
                                        -- (String.Normalize.removeDiacritics
                                        model.filter
                                     -- )
                                    )
                                    (String.toLower
                                        -- (String.Normalize.removeDiacritics
                                        haystack
                                     -- )
                                    )
                            )
                            items

                id =
                    "yafl-select-list-" ++ model.id
            in
            [ H.div
                [ HA.class "yafl-select-list"
                , HA.id id
                ]
                (List.map (viewItem config model label) filtered)

            -- TO FIX, COMMENT OUT THE LINES ABOVE AND UNCOMMENT THIS:
            --   HK.node id
            --     [ HA.class "yafl-select-list"
            --     , HA.id id
            --     ]
            --     (List.map (viewItemKeyed config model label) filtered)
            ]


viewItem : SelectConfig item -> SelectModel item -> String -> item -> H.Html (SelectMsg item)
viewItem config model label item =
    let
        itemId =
            config.toInt item

        isChecked =
            model.selected == Just itemId

        id =
            "yafl-select-item-" ++ model.id ++ "-" ++ String.fromInt itemId
    in
    H.label [ HA.class "yafl-select-item", HA.id id ]
        [ H.input
            [ HA.type_ "radio"
            , HA.name (label ++ "-radio")
            , HA.checked isChecked
            , HE.onCheck
                (\nowChecked ->
                    Selected
                        (if nowChecked then
                            Just itemId

                         else
                            Nothing
                        )
                )
            ]
            []
        , config.toHtml item
        ]


viewItemKeyed : SelectConfig item -> SelectModel item -> String -> item -> ( String, H.Html (SelectMsg item) )
viewItemKeyed config model label item =
    let
        itemId =
            config.toInt item

        isChecked =
            model.selected == Just itemId

        id =
            "yafl-select-item-" ++ model.id ++ "-" ++ String.fromInt itemId
    in
    ( id
    , H.label [ HA.class "yafl-select-item", HA.id id ]
        [ H.input
            [ HA.type_ "radio"
            , HA.name (label ++ "-radio")
            , HA.checked isChecked
            , HE.onCheck
                (\nowChecked ->
                    Selected
                        (if nowChecked then
                            Just itemId

                         else
                            Nothing
                        )
                )
            ]
            []
        , config.toHtml item
        ]
    )
