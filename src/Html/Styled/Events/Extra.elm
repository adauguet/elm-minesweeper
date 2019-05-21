module Html.Styled.Events.Extra exposing
    ( Button(..)
    , Event
    , alwaysPreventDefault
    , alwaysStopPropagation
    , alwaysStopPropagationAndPreventDefault
    , buttonDecoder
    , eventDecoder
    , onClick
    , onContextMenu
    , onMouseDown
    , onMouseUp
    )

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (custom, on)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP



-- events


onContextMenu : msg -> Attribute msg
onContextMenu message =
    custom "contextmenu" (alwaysStopPropagationAndPreventDefault message)


onMouseDown : (Button -> msg) -> Attribute msg
onMouseDown message =
    on "mousedown" (buttonDecoder message)


onMouseUp : (Button -> msg) -> Attribute msg
onMouseUp message =
    on "mouseup" (buttonDecoder message)


onClick : (Button -> msg) -> Attribute msg
onClick message =
    on "click" (buttonDecoder message)



-- Button


type Button
    = Left
    | Middle
    | Right


buttonDecoder : (Button -> msg) -> Decoder msg
buttonDecoder message =
    D.field "button" D.int
        |> D.andThen
            (\button ->
                case button of
                    0 ->
                        D.succeed (message Left)

                    1 ->
                        D.succeed (message Middle)

                    2 ->
                        D.succeed (message Right)

                    _ ->
                        D.fail "Unrecognized button code. Valid button codes are 0, 1, 2."
            )



-- Event


type alias Event msg =
    { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }


eventDecoder : Bool -> Bool -> msg -> D.Decoder (Event msg)
eventDecoder stopPropagation preventDefault message =
    D.succeed Event
        |> DP.hardcoded message
        |> DP.hardcoded stopPropagation
        |> DP.hardcoded preventDefault


alwaysStopPropagation : msg -> Decoder (Event msg)
alwaysStopPropagation message =
    eventDecoder True False message


alwaysPreventDefault : msg -> Decoder (Event msg)
alwaysPreventDefault message =
    eventDecoder False True message


alwaysStopPropagationAndPreventDefault : msg -> Decoder (Event msg)
alwaysStopPropagationAndPreventDefault message =
    eventDecoder True True message
