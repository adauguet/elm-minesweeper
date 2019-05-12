module Main exposing (main)

import Array exposing (Array)
import Browser
import Css
    exposing
        ( Color
        , alignItems
        , backgroundColor
        , bold
        , center
        , color
        , displayFlex
        , fontFamily
        , fontSize
        , fontWeight
        , height
        , justifyContent
        , monospace
        , padding
        , pct
        , px
        , rem
        , rgb
        , width
        )
import Grid exposing (Grid, randomCoordinates)
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Events.Extra exposing (onRightClick)
import Set exposing (Set)


config : { rows : Int, columns : Int, mines : Int }
config =
    { rows = 10
    , columns = 10
    , mines = 10
    }


type CellState
    = Initial
    | Flag
    | Revealed


type CellValue
    = Mine
    | Value Int


type alias Cell =
    { value : CellValue
    , coordinate : ( Int, Int )
    , state : CellState
    }


type alias Model =
    { grid : Grid Cell
    , state : State
    }


type State
    = Playing
    | Lost ( Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialCell coordinate =
            { state = Initial, coordinate = coordinate, value = Value 0 }

        grid =
            Grid.initialize config.rows config.columns initialCell
    in
    ( { grid = grid
      , state = Playing
      }
    , randomCoordinates grid config.mines NewCoordinates
    )


setValues : Grid Cell -> Grid Cell
setValues grid =
    Grid.map
        (\cell ->
            { cell
                | value =
                    case cell.value of
                        Value _ ->
                            Value (computeValue grid cell.coordinate)

                        Mine ->
                            Mine
            }
        )
        grid


minesCount : Grid Cell -> Int
minesCount grid =
    grid
        |> Grid.filter (\cell -> cell.value == Mine)
        |> List.length



-- udpate


type Msg
    = NewCoordinates (List ( Int, Int ))
    | LeftClick Cell
    | RightClick Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCoordinates coordinates ->
            ( { model | grid = setMines model.grid coordinates }, Cmd.none )

        LeftClick cell ->
            if cell.value == Mine then
                ( { model
                    | grid = Grid.set cell.coordinate { cell | state = Revealed } model.grid
                    , state = Lost cell.coordinate
                  }
                , Cmd.none
                )

            else
                let
                    nodesToReveal =
                        search model.grid [] [ cell ]
                in
                ( { model | grid = reveal model.grid nodesToReveal }, Cmd.none )

        RightClick cell ->
            case cell.state of
                Initial ->
                    ( { model | grid = Grid.set cell.coordinate { cell | state = Flag } model.grid }
                    , Cmd.none
                    )

                Flag ->
                    ( { model | grid = Grid.set cell.coordinate { cell | state = Initial } model.grid }
                    , Cmd.none
                    )

                Revealed ->
                    ( model, Cmd.none )


setMines : Grid Cell -> List ( Int, Int ) -> Grid Cell
setMines grid coordinates =
    case coordinates of
        head :: tail ->
            case Grid.get head grid of
                Just cell ->
                    let
                        newGrid =
                            Grid.set head { cell | value = Mine } grid
                    in
                    setMines newGrid tail

                Nothing ->
                    grid

        [] ->
            grid


search : Grid Cell -> List Cell -> List Cell -> List Cell
search grid handled toHandle =
    case toHandle of
        [] ->
            handled

        head :: tail ->
            case computeValue grid head.coordinate of
                0 ->
                    let
                        newNodes =
                            neighbors8 head.coordinate
                                |> Grid.getMultiple grid
                                |> List.filter (\cell -> List.member cell handled |> not)
                                |> List.filter (\cell -> List.member cell tail |> not)
                    in
                    search grid (head :: handled) (tail ++ newNodes)

                _ ->
                    search grid (head :: handled) tail


reveal : Grid Cell -> List Cell -> Grid Cell
reveal grid cells =
    case cells of
        [] ->
            grid

        head :: tail ->
            let
                newGrid =
                    Grid.set head.coordinate { head | state = Revealed } grid
            in
            reveal newGrid tail


neighbors8 : ( Int, Int ) -> List ( Int, Int )
neighbors8 ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


computeValue : Grid Cell -> ( Int, Int ) -> Int
computeValue grid coordinate =
    neighbors8 coordinate
        |> Grid.getMultiple grid
        |> List.filter (\cell -> cell.value == Mine)
        |> List.length



-- view


view : Model -> Html Msg
view model =
    model.grid
        |> Grid.map (cellView model)
        |> Array.toList
        |> List.map row
        |> div [ css [ displayFlex ] ]


row : Array (Html msg) -> Html msg
row elements =
    elements
        |> Array.toList
        |> div []


type alias NodeStyle =
    { backgroundColor : Color
    , title : String
    , color : Color
    }


colorForValue : Int -> Color
colorForValue value =
    case value of
        1 ->
            rgb 11 36 251

        2 ->
            rgb 14 122 17

        3 ->
            rgb 252 13 27

        4 ->
            rgb 2 11 121

        5 ->
            rgb 122 3 7

        6 ->
            rgb 16 123 122

        8 ->
            rgb 123 123 123

        _ ->
            rgb 0 0 0


titleForValue : Int -> String
titleForValue value =
    case value of
        0 ->
            ""

        i ->
            String.fromInt i


backgroundGray : Color
backgroundGray =
    rgb 189 189 189


cellView : Model -> Cell -> Html Msg
cellView model cell =
    case model.state of
        Playing ->
            case cell.state of
                Initial ->
                    initialView cell

                Revealed ->
                    let
                        value =
                            computeValue model.grid cell.coordinate
                    in
                    revealedView
                        { backgroundColor = backgroundGray
                        , title = titleForValue value
                        , color = colorForValue value
                        }

                Flag ->
                    flagView cell

        Lost ( _, _ ) ->
            case ( cell.state, cell.value ) of
                ( Initial, Value _ ) ->
                    initialViewSilent

                ( Initial, Mine ) ->
                    bombView

                ( Revealed, Value _ ) ->
                    let
                        value =
                            computeValue model.grid cell.coordinate
                    in
                    revealedView
                        { backgroundColor = backgroundGray
                        , title = titleForValue value
                        , color = colorForValue value
                        }

                ( Revealed, Mine ) ->
                    bombClickedView

                ( Flag, Mine ) ->
                    flagViewSilent

                ( Flag, Value _ ) ->
                    errorView


initialView : Cell -> Html Msg
initialView cell =
    div
        [ css
            [ backgroundColor (rgb 100 100 100)
            , width (pct 100)
            , height (pct 100)
            ]
        , onClick (LeftClick cell)
        , onRightClick (RightClick cell)
        ]
        []
        |> tileView


initialViewSilent : Html Msg
initialViewSilent =
    div
        [ css
            [ backgroundColor (rgb 100 100 100)
            , width (pct 100)
            , height (pct 100)
            ]
        ]
        []
        |> tileView


bombView : Html Msg
bombView =
    revealedView
        { backgroundColor = backgroundGray
        , title = "💣"
        , color = rgb 0 0 0
        }


bombClickedView : Html Msg
bombClickedView =
    revealedView
        { backgroundColor = rgb 255 0 0
        , title = "💣"
        , color = rgb 0 0 0
        }


flagView : Cell -> Html Msg
flagView cell =
    div
        [ css
            [ backgroundColor (rgb 100 100 100)
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , fontFamily monospace
            , fontWeight bold
            , fontSize (px 18)
            ]
        , onRightClick (RightClick cell)
        ]
        [ text "⚑" ]
        |> tileView


flagViewSilent : Html Msg
flagViewSilent =
    div
        [ css
            [ backgroundColor (rgb 100 100 100)
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , fontFamily monospace
            , fontWeight bold
            , fontSize (px 18)
            ]
        ]
        [ text "⚑" ]
        |> tileView


errorView : Html Msg
errorView =
    div
        [ css
            [ backgroundColor (rgb 100 100 100)
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , fontFamily monospace
            , fontWeight bold
            , fontSize (px 18)
            ]
        ]
        [ text "❌" ]
        |> tileView


revealedView : NodeStyle -> Html msg
revealedView style =
    div
        [ css
            [ backgroundColor style.backgroundColor
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , color style.color
            , fontFamily monospace
            , fontWeight bold
            , fontSize (px 18)
            ]
        ]
        [ text style.title ]
        |> tileView


tileView : Html msg -> Html msg
tileView content =
    div
        [ css
            [ height (rem 2)
            , width (rem 2)
            , padding (rem 0.1)
            ]
        ]
        [ content ]



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
