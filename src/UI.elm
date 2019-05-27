module UI exposing
    ( bottomBorder
    , digitStyles
    , faceStyles
    , middleBorder
    , ribbonBorder
    , row
    , tileStyles
    , tileView
    , topBorder
    )

import Css
    exposing
        ( Style
        , backgroundImage
        , backgroundPosition2
        , displayFlex
        , height
        , px
        , url
        , width
        )
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)



-- sprite


spriteView : ( Float, Float ) -> Float -> Float -> Html msg
spriteView offset width height =
    div [ css (spriteStyles offset width height) ] []


digitStyles : Float -> List Style
digitStyles xOffset =
    spriteStyles ( xOffset, 0 ) 13 23


tileStyles : ( Float, Float ) -> List Style
tileStyles offset =
    spriteStyles offset 16 16


faceStyles : Float -> List Style
faceStyles xOffset =
    spriteStyles ( xOffset, -55 ) 26 26


spriteStyles : ( Float, Float ) -> Float -> Float -> List Style
spriteStyles ( x, y ) w h =
    [ backgroundImage (url "assets/sprite.gif")
    , backgroundPosition2 (px x) (px y)
    , width (px w)
    , height (px h)
    ]



-- helpers


tileView : ( Float, Float ) -> List (Attribute msg) -> Html msg
tileView offset events =
    div (css (tileStyles offset) :: events) []


row : List (Html msg) -> Html msg
row elements =
    div [ css [ displayFlex ] ]
        (sideView :: elements ++ [ sideView ])



-- decorative


topLeftCorner : Html msg
topLeftCorner =
    spriteView ( 0, -81 ) 10 10


topRightCorner : Html msg
topRightCorner =
    spriteView ( -10, -81 ) 10 10


topBorderCell : Html msg
topBorderCell =
    spriteView ( -40, -81 ) 16 10


bottomLeftCorner : Html msg
bottomLeftCorner =
    spriteView ( -20, -81 ) 10 10


bottomRightCorner : Html msg
bottomRightCorner =
    spriteView ( -30, -81 ) 10 10


topBorder : Int -> Html msg
topBorder count =
    div
        [ css [ displayFlex ] ]
        (topLeftCorner :: List.repeat count topBorderCell ++ [ topRightCorner ])


middleLeftCorner : Html msg
middleLeftCorner =
    spriteView ( -56, -81 ) 10 10


middleRightCorner : Html msg
middleRightCorner =
    spriteView ( -66, -81 ) 10 10


middleBorder : Int -> Html msg
middleBorder count =
    div
        [ css [ displayFlex ] ]
        (middleLeftCorner :: List.repeat count topBorderCell ++ [ middleRightCorner ])


bottomBorder : Int -> Html msg
bottomBorder count =
    div
        [ css [ displayFlex ] ]
        (bottomLeftCorner :: List.repeat count topBorderCell ++ [ bottomRightCorner ])


ribbonBorder : Html msg
ribbonBorder =
    spriteView ( -134, -39 ) 10 32


sideView : Html msg
sideView =
    spriteView ( -134, -39 ) 10 16
