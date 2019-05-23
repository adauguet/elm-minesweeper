module Counter exposing (bound, counter, toDigitChar)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)


counter : Int -> Float -> Float -> Html msg
counter int leftMargin rightMargin =
    let
        ( char1, char2, char3 ) =
            toDigitChar int
    in
    div [ css [ displayFlex ] ]
        [ div [ css ([ margin4 (px 4) zero (px 5) (px leftMargin) ] ++ digitStyle char1) ] []
        , div [ css ([ margin4 (px 4) zero (px 5) zero ] ++ digitStyle char2) ] []
        , div [ css ([ margin4 (px 4) (px rightMargin) (px 5) zero ] ++ digitStyle char3) ] []
        ]


toDigitChar : Int -> ( Char, Char, Char )
toDigitChar int =
    let
        int_ =
            bound int -99 999
    in
    if int_ < 0 then
        case
            int_
                |> abs
                |> String.fromInt
                |> String.toList
        of
            [] ->
                ( '-', '-', '-' )

            [ a ] ->
                ( '-', '0', a )

            a :: b :: _ ->
                ( '-', a, b )

    else
        case String.toList (String.fromInt int_) of
            [] ->
                ( '-', '-', '-' )

            [ a ] ->
                ( '0', '0', a )

            [ a, b ] ->
                ( '0', a, b )

            a :: b :: c :: _ ->
                ( a, b, c )


bound : Int -> Int -> Int -> Int
bound int low high =
    min high (max int low)


digitStyle : Char -> List Style
digitStyle char =
    let
        styles : ( Float, Float ) -> List Style
        styles ( x, y ) =
            [ backgroundImage (url "assets/sprite.gif")
            , backgroundPosition2 (px x) (px y)
            , width (px 13)
            , height (px 23)
            ]
    in
    case char of
        '0' ->
            styles ( 0, 0 )

        '1' ->
            styles ( -13, 0 )

        '2' ->
            styles ( -26, 0 )

        '3' ->
            styles ( -39, 0 )

        '4' ->
            styles ( -52, 0 )

        '5' ->
            styles ( -65, 0 )

        '6' ->
            styles ( -78, 0 )

        '7' ->
            styles ( -91, 0 )

        '8' ->
            styles ( -104, 0 )

        '9' ->
            styles ( -117, 0 )

        '-' ->
            styles ( -130, 0 )

        _ ->
            []
