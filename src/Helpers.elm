module Helpers exposing (scrollElementToMiddle)

import Browser.Dom
import Task


scrollElementToMiddle : { elementId : String, containerId : String } -> Task.Task Browser.Dom.Error ()
scrollElementToMiddle { elementId, containerId } =
    Browser.Dom.getElement elementId
        |> Task.andThen
            (\elementInfo ->
                Browser.Dom.getElement containerId
                    |> Task.andThen
                        (\containerInfo ->
                            Browser.Dom.getViewportOf containerId
                                |> Task.andThen
                                    (\containerViewport ->
                                        let
                                            newViewportY =
                                                containerViewport.viewport.y
                                                    - (0.5 * containerViewport.viewport.height)
                                                    + (0.5 * elementInfo.element.height)
                                                    + (elementInfo.element.y - containerInfo.element.y)
                                        in
                                        Browser.Dom.setViewportOf containerId 0 newViewportY
                                    )
                        )
            )
