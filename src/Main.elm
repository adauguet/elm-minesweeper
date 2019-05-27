module Main exposing (main)

import Browser
import Browser.Events as Events
import Cell exposing (Cell, CellState(..), CellValue(..))
import Counter exposing (counter)
import Css exposing (backgroundColor, display, displayFlex, inlineBlock, margin4, px, rgb)
import Grid exposing (Grid, randomCoordinates)
import Html.Styled exposing (Html, div, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onMouseOut, onMouseOver)
import Html.Styled.Events.Extra exposing (Button(..), onClick, onContextMenu, onMouseDown, onMouseUp)
import Json.Decode as D
import Random
import Time exposing (Posix)
import UI
    exposing
        ( bottomBorder
        , faceStyles
        , middleBorder
        , ribbonBorder
        , row
        , tileView
        , topBorder
        )


config : { rows : Int, columns : Int, mines : Int }
config =
    { rows = 16
    , columns = 30
    , mines = 99
    }


type FaceState
    = FaceStateDefault
    | FaceStateLost
    | FaceStateWon
    | FaceStateSurprise
    | FaceStateDown


type State
    = Playing Bool
    | Lost ( Int, Int )
    | Won


type alias Model =
    { grid : Grid Cell
    , state : State
    , faceState : FaceState
    , isMouseDown : Bool
    , flags : Int
    , seconds : Int
    }


initialGrid : Grid Cell
initialGrid =
    let
        initialCell coordinate =
            { state = Initial, coordinate = coordinate, value = Value 0, isDown = False }
    in
    Grid.initialize config.rows config.columns initialCell


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = initialGrid
      , state = Playing False
      , faceState = FaceStateDefault
      , isMouseDown = False
      , flags = 0
      , seconds = 0
      }
    , Cmd.none
    )


setMines : List ( Int, Int ) -> Grid Cell -> Grid Cell
setMines coordinates grid =
    case coordinates of
        [] ->
            grid

        head :: tail ->
            let
                newGrid =
                    setMine head grid
            in
            setMines tail newGrid


setMine : ( Int, Int ) -> Grid Cell -> Grid Cell
setMine coordinate grid =
    case Grid.get coordinate grid of
        Just cell ->
            Grid.set coordinate { cell | value = Mine } grid

        Nothing ->
            grid


computeValues : Grid Cell -> Grid Cell
computeValues grid =
    let
        setValue : Cell -> Cell
        setValue cell =
            { cell
                | value =
                    case cell.value of
                        Value _ ->
                            Value (computeValue grid cell.coordinate)

                        Mine ->
                            Mine
            }
    in
    Grid.map setValue grid


computeValue : Grid Cell -> ( Int, Int ) -> Int
computeValue grid coordinate =
    let
        neighbors =
            getNeighbors coordinate
                |> Grid.getMultiple grid
    in
    neighbors
        |> List.filter (\cell -> cell.value == Mine)
        |> List.length


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
    = GotMinesCoordinates ( Int, Int ) (List ( Int, Int ))
    | OnCellMouseDown Cell Button
    | OnCellMouseOver Cell
    | OnCellMouseOut Cell
    | OnCellMouseUp Cell Button
    | OnFaceMouseDown Button
    | OnFaceMouseOut
    | OnFaceClick Button
    | OnMouseDown
    | OnMouseUp
    | Tic Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMinesCoordinates coordinate coordinates ->
            let
                newGrid =
                    model.grid
                        |> setMines coordinates
                        |> computeValues

                newModel =
                    { model | grid = newGrid, state = Playing True }
            in
            case Grid.get coordinate newGrid of
                Just cell ->
                    handleCellLeftClick newModel cell

                Nothing ->
                    ( model, Cmd.none )

        OnCellMouseDown cell Left ->
            ( { model
                | grid = setCellDown model.grid cell True
                , faceState = FaceStateSurprise
              }
            , Cmd.none
            )

        OnCellMouseDown _ Middle ->
            ( { model | faceState = FaceStateSurprise }, Cmd.none )

        OnCellMouseDown cell Right ->
            case cell.state of
                Flag ->
                    ( { model | grid = unFlagCell model.grid cell, flags = model.flags - 1 }, Cmd.none )

                Initial ->
                    ( { model | grid = flagCell model.grid cell, flags = model.flags + 1 }, Cmd.none )

                Revealed ->
                    ( model, Cmd.none )

        OnCellMouseOver cell ->
            if model.isMouseDown then
                ( { model | grid = setCellDown model.grid cell True }, Cmd.none )

            else
                ( model, Cmd.none )

        OnCellMouseOut cell ->
            ( { model | grid = setCellDown model.grid cell False }, Cmd.none )

        OnCellMouseUp cell Left ->
            handleCellLeftClick model cell

        OnCellMouseUp _ _ ->
            ( model, Cmd.none )

        OnFaceMouseDown _ ->
            ( { model | faceState = FaceStateDown }, Cmd.none )

        OnFaceClick Left ->
            init ()

        OnFaceClick _ ->
            ( model, Cmd.none )

        OnFaceMouseOut ->
            if model.faceState == FaceStateDown then
                ( { model | faceState = FaceStateDefault }, Cmd.none )

            else
                ( model, Cmd.none )

        OnMouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        OnMouseUp ->
            ( { model | isMouseDown = False, faceState = FaceStateDefault }, Cmd.none )

        Tic _ ->
            ( { model | seconds = model.seconds + 1 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleCellLeftClick : Model -> Cell -> ( Model, Cmd Msg )
handleCellLeftClick model cell =
    case model.state of
        Playing True ->
            case cell.state of
                Initial ->
                    if cell.value == Mine then
                        ( { model
                            | grid = Grid.set cell.coordinate { cell | state = Revealed } model.grid
                            , state = Lost cell.coordinate
                            , faceState = FaceStateLost
                          }
                        , Cmd.none
                        )

                    else
                        let
                            newGrid =
                                search model.grid [ cell ]
                        in
                        if hasWon newGrid then
                            ( { model
                                | grid = newGrid
                                , state = Won
                                , faceState = FaceStateWon
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | grid = newGrid
                                , faceState = FaceStateDefault
                              }
                            , Cmd.none
                            )

                Flag ->
                    ( model, Cmd.none )

                Revealed ->
                    ( model, Cmd.none )

        Playing False ->
            let
                avoid =
                    cell.coordinate :: getNeighbors cell.coordinate
            in
            ( model, Random.generate (GotMinesCoordinates cell.coordinate) (randomCoordinates config.mines model.grid avoid) )

        _ ->
            ( model, Cmd.none )



-- search and reveal algorithm


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
                    search (revealCell grid head) (tail ++ newCells)

                ( Initial, Value _ ) ->
                    search (revealCell grid head) tail

                ( Revealed, Value _ ) ->
                    search grid tail

                ( Flag, Value _ ) ->
                    search grid tail

                ( _, Mine ) ->
                    search grid tail


revealCell : Grid Cell -> Cell -> Grid Cell
revealCell grid cell =
    Grid.set cell.coordinate { cell | state = Revealed } grid


flagCell : Grid Cell -> Cell -> Grid Cell
flagCell grid cell =
    Grid.set cell.coordinate { cell | state = Flag } grid


unFlagCell : Grid Cell -> Cell -> Grid Cell
unFlagCell grid cell =
    Grid.set cell.coordinate { cell | state = Initial } grid


setCellDown : Grid Cell -> Cell -> Bool -> Grid Cell
setCellDown grid cell isDown =
    Grid.set cell.coordinate { cell | isDown = isDown } grid


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
    div
        [ css
            [ display inlineBlock
            , backgroundColor (rgb 192 192 192)
            ]
        , onContextMenu NoOp
        ]
        ([ topBorder config.columns
         , ribbon config.columns (config.mines - model.flags) model.seconds model.faceState
         , middleBorder config.columns
         ]
            ++ rows
            ++ [ bottomBorder config.columns ]
        )


ribbon : Int -> Int -> Int -> FaceState -> Html Msg
ribbon count flagCount seconds faceState =
    div
        [ css [ displayFlex ] ]
        [ ribbonBorder, counter flagCount 6 0, faceButton count faceState, counter seconds 0 6, ribbonBorder ]



-- face button


faceButton : Int -> FaceState -> Html Msg
faceButton count faceState =
    let
        amount =
            (16 * toFloat count - 26) / 2 - 45
    in
    div []
        [ div
            [ css
                (margin4 (px 3) (px amount) (px 3) (px amount)
                    :: (case faceState of
                            FaceStateDefault ->
                                faceStyles 0

                            FaceStateDown ->
                                faceStyles -26

                            FaceStateSurprise ->
                                faceStyles -52

                            FaceStateLost ->
                                faceStyles -78

                            FaceStateWon ->
                                faceStyles -104
                       )
                )
            , onClick OnFaceClick
            , onMouseDown OnFaceMouseDown
            , onMouseOut OnFaceMouseOut
            ]
            []
        ]



-- cells


cellView : Model -> Cell -> Html Msg
cellView model cell =
    case model.state of
        Playing _ ->
            case cell.state of
                Initial ->
                    if cell.isDown then
                        clickedView cell

                    else
                        initialView cell

                Revealed ->
                    case cell.value of
                        -- this case should not happen
                        Mine ->
                            div [] []

                        Value value ->
                            valueView value

                Flag ->
                    flagView cell

        Lost _ ->
            case ( cell.state, cell.value ) of
                ( Initial, Value _ ) ->
                    initialViewSilent

                ( Initial, Mine ) ->
                    mineView

                ( Revealed, Value value ) ->
                    valueView value

                ( Revealed, Mine ) ->
                    mineClickedView

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
        [ onMouseDown (OnCellMouseDown cell)
        , onMouseUp (OnCellMouseUp cell)
        , onMouseOver (OnCellMouseOver cell)
        , onMouseOut (OnCellMouseOut cell)
        ]


clickedView : Cell -> Html Msg
clickedView cell =
    tileView ( 0, -23 )
        [ onMouseDown (OnCellMouseDown cell)
        , onMouseUp (OnCellMouseUp cell)
        , onMouseOver (OnCellMouseOver cell)
        , onMouseOut (OnCellMouseOut cell)
        ]


initialViewSilent : Html msg
initialViewSilent =
    tileView ( 0, -39 ) []


mineView : Html msg
mineView =
    tileView ( -64, -39 ) []


mineClickedView : Html msg
mineClickedView =
    tileView ( -32, -39 ) []


flagView : Cell -> Html Msg
flagView cell =
    tileView ( -16, -39 )
        [ onMouseDown (OnCellMouseDown cell) ]


flagViewSilent : Html msg
flagViewSilent =
    tileView ( -16, -39 ) []


errorView : Html msg
errorView =
    tileView ( -48, -39 ) []


valueView : Int -> Html msg
valueView value =
    tileView ( -(16 * toFloat value), -23 ) []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onMouseUp (D.succeed OnMouseUp)
        , Events.onMouseDown (D.succeed OnMouseDown)
        , if model.state == Playing True then
            Time.every 1000 Tic

          else
            Sub.none
        ]



-- main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
