module Html.Styled.Events.Extra exposing (onRightClick)

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (custom)
import Json.Decode as D


onRightClick : msg -> Attribute msg
onRightClick message =
    custom "contextmenu" (eventDecoder message)


type alias Event msg =
    { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }


eventDecoder : msg -> D.Decoder (Event msg)
eventDecoder message =
    D.map3 Event
        (D.succeed message)
        (D.succeed True)
        (D.succeed True)
