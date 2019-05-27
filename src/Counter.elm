module Counter exposing (counter, toDigitChar)

import Css
    exposing
        ( Style
        , displayFlex
        , margin4
        , px
        , zero
        )
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import UI


counter : Int -> Float -> Float -> Html msg
counter int leftMargin rightMargin =
    let
        ( char1, char2, char3 ) =
            toDigitChar int
    in
    div [ css [ displayFlex ] ]
        [ div [ css (margin4 (px 4) zero (px 5) (px leftMargin) :: digitStyles char1) ] []
        , div [ css (margin4 (px 4) zero (px 5) zero :: digitStyles char2) ] []
        , div [ css (margin4 (px 4) (px rightMargin) (px 5) zero :: digitStyles char3) ] []
        ]


toDigitChar : Int -> ( Char, Char, Char )
toDigitChar int =
    let
        int_ =
            clamp -99 999 int
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


digitStyles : Char -> List Style
digitStyles char =
    case char of
        '0' ->
            UI.digitStyles 0

        '1' ->
            UI.digitStyles -13

        '2' ->
            UI.digitStyles -26

        '3' ->
            UI.digitStyles -39

        '4' ->
            UI.digitStyles -52

        '5' ->
            UI.digitStyles -65

        '6' ->
            UI.digitStyles -78

        '7' ->
            UI.digitStyles -91

        '8' ->
            UI.digitStyles -104

        '9' ->
            UI.digitStyles -117

        '-' ->
            UI.digitStyles -130

        _ ->
            []
