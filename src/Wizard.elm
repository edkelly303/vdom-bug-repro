module Wizard exposing (view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Yafl as Y


view :
    (Y.Msg formMsg -> msg)
    -> msg
    -> Y.Field formModel formMsg id widgetMsg input output
    -> Y.Model formModel output
    -> List (H.Html msg)
view toMsg submitMsg form model =
    let
        { stepView, backMsg, nextMsg } =
            Y.viewWizard form model

        step =
            List.map (H.map toMsg) stepView

        controls =
            viewControls toMsg backMsg nextMsg
    in
    [ H.form [ HE.onSubmit submitMsg ]
        (step ++ controls)
    ]


viewControls :
    (formMsg -> msg)
    -> Maybe formMsg
    -> Maybe formMsg
    -> List (H.Html msg)
viewControls toMsg backMsg nextMsg =
    [ H.span []
        [ case backMsg of
            Nothing ->
                H.button
                    [ HA.type_ "button"
                    , HA.class "save-object"
                    , HA.disabled True
                    ]
                    [ H.text "Back" ]

            Just msg ->
                H.button
                    [ HA.type_ "button"
                    , HA.class "save-object"
                    , HE.onClick (toMsg msg)
                    ]
                    [ H.text "Back" ]
        , case nextMsg of
            Nothing ->
                H.button
                    [ HA.type_ "submit"
                    , HA.class "save-object"
                    ]
                    [ H.text "Save" ]

            Just msg ->
                H.button
                    [ HA.type_ "button"
                    , HA.class "save-object"
                    , HE.onClick (toMsg msg)
                    ]
                    [ H.text "Next" ]
        ]
    ]
