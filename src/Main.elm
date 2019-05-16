module Main exposing (main)

import Array exposing (Array)
import Browser
import Css
    exposing
        ( backgroundColor
        , backgroundImage
        , backgroundPosition2
        , displayFlex
        , height
        , margin4
        , px
        , rgb
        , url
        , width
        )
import Grid exposing (Grid, randomCoordinate)
import Html.Styled exposing (Attribute, Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Events.Extra exposing (onRightClick)
import List.Split as List exposing (chunksOfLeft)
import Set exposing (Set)


config : { rows : Int, columns : Int, mines : Int }
config =
    { rows = 16
    , columns = 30
    , mines = 99
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
    = Building
    | Playing
    | Lost ( Int, Int )
    | Won


initialGrid : Grid Cell
initialGrid =
    let
        initialCell coordinate =
            { state = Initial, coordinate = coordinate, value = Value 0 }
    in
    Grid.initialize config.rows config.columns initialCell


init : () -> ( Model, Cmd Msg )
init _ =
    let
        grid =
            initialGrid
    in
    ( { grid = grid
      , state = Building
      }
    , randomCoordinate grid NewCoordinate
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


computeValue : Grid Cell -> ( Int, Int ) -> Int
computeValue grid coordinate =
    let
        -- _ =
        --     Debug.log "coordinate" coordinate
        neighbors =
            getNeighbors coordinate
                |> Grid.getMultiple grid

        -- _ =
        --     Debug.log "neighbors" neighbors
        value =
            neighbors
                |> List.filter (\cell -> cell.value == Mine)
                |> List.length

        -- _ =
        --     Debug.log "value" value
    in
    value


hasWon : Grid Cell -> Bool
hasWon grid =
    let
        mines =
            Grid.filter (\cell -> cell.value == Mine) grid

        notRevealed =
            Grid.filter (\cell -> cell.state /= Revealed) grid
    in
    mines == notRevealed



-- udpate


type Msg
    = NewCoordinate ( Int, Int )
    | LeftClick Cell
    | RightClick Cell
    | ClickFace


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCoordinate coordinate ->
            let
                -- _ =
                --     Debug.log "coordinate" coordinate
                newGrid =
                    case Grid.get coordinate model.grid of
                        Just cell ->
                            Grid.set coordinate { cell | value = Mine } model.grid

                        Nothing ->
                            model.grid

                count =
                    newGrid
                        |> Grid.filter (\cell -> cell.value == Mine)
                        |> List.length
            in
            if count < config.mines then
                ( { model | grid = newGrid }, randomCoordinate newGrid NewCoordinate )

            else
                ( { model | grid = setValues newGrid, state = Playing }, Cmd.none )

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
                    newGrid =
                        search model.grid [ cell ]

                    newState =
                        if hasWon newGrid then
                            Won

                        else
                            model.state
                in
                ( { model | grid = newGrid, state = newState }, Cmd.none )

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

        ClickFace ->
            let
                grid =
                    initialGrid
            in
            ( { grid = grid
              , state = Building
              }
            , randomCoordinate grid NewCoordinate
            )


search : Grid Cell -> List Cell -> Grid Cell
search grid cells =
    case cells of
        [] ->
            grid

        head :: tail ->
            case ( head.state, head.value ) of
                ( Initial, Value 0 ) ->
                    let
                        newCells =
                            head.coordinate
                                |> getNeighbors
                                |> Grid.getMultiple grid
                                |> List.filter (\cell -> cell.state == Initial && not (List.member cell tail))
                    in
                    search (reveal grid head) (tail ++ newCells)

                ( Initial, Value _ ) ->
                    search (reveal grid head) tail

                ( Revealed, Value _ ) ->
                    search grid tail

                ( Flag, Value _ ) ->
                    search grid tail

                ( _, Mine ) ->
                    search grid tail


reveal : Grid Cell -> Cell -> Grid Cell
reveal grid cell =
    Grid.set cell.coordinate { cell | state = Revealed } grid


getNeighbors : ( Int, Int ) -> List ( Int, Int )
getNeighbors ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]



-- view


view : Model -> Html Msg
view model =
    let
        rows =
            model.grid
                |> Grid.map (cellView model)
                |> Grid.toLists
                |> List.map row
    in
    div []
        (topBorder config.columns
            :: [ ribbon config.columns ]
            ++ [ middleBorder config.columns ]
            ++ rows
            ++ [ bottomBorder config.columns ]
        )


row : List (Html msg) -> Html msg
row elements =
    div [ css [ displayFlex ] ]
        (sideView :: elements ++ [ sideView ])


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


spriteView : ( Float, Float ) -> Float -> Float -> Html msg
spriteView ( x, y ) w h =
    div
        [ css
            [ backgroundImage (url "/assets/sprite.gif")
            , backgroundPosition2 (px x) (px y)
            , width (px w)
            , height (px h)
            ]
        ]
        []


faceButton : Int -> Html Msg
faceButton count =
    let
        amount =
            (16 * toFloat count - 26) / 2
    in
    div
        [ css [ backgroundColor (rgb 192 192 192) ]
        ]
        [ div
            [ css
                [ backgroundImage (url "/assets/sprite.gif")
                , backgroundPosition2 (px 0) (px -55)
                , width (px 26)
                , height (px 26)
                , margin4 (px 3) (px amount) (px 3) (px amount)
                ]
            , onClick ClickFace
            ]
            []
        ]


ribbon : Int -> Html Msg
ribbon count =
    div
        [ css [ displayFlex ] ]
        [ ribbonBorder, faceButton count, ribbonBorder ]


sideView : Html msg
sideView =
    spriteView ( -134, -39 ) 10 16


cellView : Model -> Cell -> Html Msg
cellView model cell =
    case model.state of
        Building ->
            initialViewSilent

        Playing ->
            case cell.state of
                Initial ->
                    initialView cell

                Revealed ->
                    case cell.value of
                        Mine ->
                            div [] []

                        Value value ->
                            valueView value

                Flag ->
                    flagView cell

        Lost ( _, _ ) ->
            case ( cell.state, cell.value ) of
                ( Initial, Value _ ) ->
                    initialViewSilent

                ( Initial, Mine ) ->
                    bombView

                ( Revealed, Value value ) ->
                    valueView value

                ( Revealed, Mine ) ->
                    bombClickedView

                ( Flag, Mine ) ->
                    flagViewSilent

                ( Flag, Value _ ) ->
                    errorView

        Won ->
            case cell.value of
                Value value ->
                    valueView value

                Mine ->
                    flagViewSilent


initialView : Cell -> Html Msg
initialView cell =
    tileView ( 0, -39 )
        [ onClick (LeftClick cell)
        , onRightClick (RightClick cell)
        ]


initialViewSilent : Html Msg
initialViewSilent =
    tileView ( 0, -39 ) []


bombView : Html Msg
bombView =
    tileView ( -64, -39 ) []


bombClickedView : Html Msg
bombClickedView =
    tileView ( -32, -39 ) []


flagView : Cell -> Html Msg
flagView cell =
    tileView ( -16, -39 ) [ onRightClick (RightClick cell) ]


flagViewSilent : Html Msg
flagViewSilent =
    tileView ( -16, -39 ) []


errorView : Html Msg
errorView =
    tileView ( -48, -39 ) []


valueView : Int -> Html Msg
valueView value =
    tileView ( -(16 * toFloat value), -23 ) []


tileView : ( Float, Float ) -> List (Attribute msg) -> Html msg
tileView ( dx, dy ) events =
    let
        attributes =
            css
                [ backgroundImage (url "/assets/sprite.gif")
                , backgroundPosition2 (px dx) (px dy)
                , height (px 16)
                , width (px 16)
                ]
                :: events
    in
    div attributes []



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
