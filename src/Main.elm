module Main exposing (main)

import Browser
import Browser.Events as Events
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
import Html.Styled.Events exposing (onMouseOut, onMouseOver)
import Html.Styled.Events.Extra exposing (Button(..), onClick, onContextMenu, onMouseDown, onMouseUp)
import Json.Decode as D


config : { rows : Int, columns : Int, mines : Int }
config =
    { rows = 16
    , columns = 30
    , mines = 99
    }


type CellValue
    = Mine
    | Value Int


type CellState
    = Initial
    | Flag
    | Revealed


type alias Cell =
    { value : CellValue
    , coordinate : ( Int, Int )
    , state : CellState
    , isDown : Bool
    }


type FaceState
    = FaceStateUp FaceStateUp
    | FaceStateDown


type State
    = Building
    | Playing
    | Lost ( Int, Int )
    | Won


type FaceStateUp
    = FaceStateUpDefault
    | FaceStateUpLost
    | FaceStateUpWon
    | FaceStateUpSurprise


type alias Model =
    { grid : Grid Cell
    , state : State
    , faceState : FaceState
    , isMouseDown : Bool
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
    let
        grid =
            initialGrid
    in
    ( { grid = grid
      , state = Building
      , faceState = FaceStateUp FaceStateUpDefault
      , isMouseDown = False
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
        neighbors =
            getNeighbors coordinate
                |> Grid.getMultiple grid

        value =
            neighbors
                |> List.filter (\cell -> cell.value == Mine)
                |> List.length
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
    | OnCellMouseDown Cell Button
    | OnCellMouseOver Cell
    | OnCellMouseOut Cell
    | OnCellMouseClick Cell Button
    | OnCellMouseUp Cell Button
    | OnFaceMouseDown Button
    | OnFaceMouseOut
    | OnFaceClick Button
    | OnMouseDown
    | OnMouseUp
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCoordinate coordinate ->
            let
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

        OnCellMouseDown cell Left ->
            ( { model
                | grid = setCellDown model.grid cell True
                , faceState = FaceStateUp FaceStateUpSurprise
              }
            , Cmd.none
            )

        OnCellMouseDown _ Middle ->
            ( { model | faceState = FaceStateUp FaceStateUpSurprise }, Cmd.none )

        OnCellMouseDown cell Right ->
            case cell.state of
                Flag ->
                    ( { model | grid = unFlagCell model.grid cell }, Cmd.none )

                Initial ->
                    ( { model | grid = flagCell model.grid cell }, Cmd.none )

                Revealed ->
                    ( model, Cmd.none )

        OnCellMouseClick cell Left ->
            handleCellLeftClick model cell

        OnCellMouseClick _ Middle ->
            ( model, Cmd.none )

        OnCellMouseClick _ Right ->
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

        OnCellMouseUp _ Middle ->
            ( model, Cmd.none )

        OnCellMouseUp _ Right ->
            ( model, Cmd.none )

        OnFaceMouseDown _ ->
            ( { model | faceState = FaceStateDown }, Cmd.none )

        OnFaceClick Left ->
            init ()

        OnFaceClick Middle ->
            ( model, Cmd.none )

        OnFaceClick Right ->
            ( model, Cmd.none )

        OnFaceMouseOut ->
            if model.faceState == FaceStateDown then
                ( { model | faceState = FaceStateUp FaceStateUpDefault }, Cmd.none )

            else
                ( model, Cmd.none )

        OnMouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        OnMouseUp ->
            ( { model | isMouseDown = False, faceState = FaceStateUp FaceStateUpDefault }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleCellLeftClick : Model -> Cell -> ( Model, Cmd Msg )
handleCellLeftClick model cell =
    case cell.state of
        Initial ->
            if cell.value == Mine then
                ( { model
                    | grid = Grid.set cell.coordinate { cell | state = Revealed } model.grid
                    , state = Lost cell.coordinate
                    , faceState = FaceStateUp FaceStateUpLost
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
                        , faceState = FaceStateUp FaceStateUpWon
                      }
                    , Cmd.none
                    )

                else
                    ( { model
                        | grid = newGrid
                        , faceState = FaceStateUp FaceStateUpDefault
                      }
                    , Cmd.none
                    )

        Flag ->
            ( model, Cmd.none )

        Revealed ->
            ( model, Cmd.none )



-- reveal algorithm


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
    div []
        (topBorder config.columns
            :: ribbon config.columns model.faceState
            :: middleBorder config.columns
            :: rows
            ++ [ bottomBorder config.columns ]
        )


ribbon : Int -> FaceState -> Html Msg
ribbon count faceState =
    div
        [ css [ displayFlex ] ]
        [ ribbonBorder, faceButton count faceState, ribbonBorder ]


row : List (Html msg) -> Html msg
row elements =
    div [ css [ displayFlex ] ]
        (sideView :: elements ++ [ sideView ])



-- face button


faceButton : Int -> FaceState -> Html Msg
faceButton count faceState =
    let
        amount =
            (16 * toFloat count - 26) / 2
    in
    div
        [ css [ backgroundColor (rgb 192 192 192) ]
        ]
        [ div
            [ css [ margin4 (px 3) (px amount) (px 3) (px amount) ]
            , case faceState of
                FaceStateUp FaceStateUpDefault ->
                    spriteAttributes ( 0, -55 ) 26 26

                FaceStateDown ->
                    spriteAttributes ( -26, -55 ) 26 26

                FaceStateUp FaceStateUpSurprise ->
                    spriteAttributes ( -52, -55 ) 26 26

                FaceStateUp FaceStateUpLost ->
                    spriteAttributes ( -78, -55 ) 26 26

                FaceStateUp FaceStateUpWon ->
                    spriteAttributes ( -104, -55 ) 26 26
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
        Building ->
            initialViewSilent

        Playing ->
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
        [ onMouseDown (OnCellMouseDown cell)
        , onMouseOver (OnCellMouseOver cell)
        , onMouseOut (OnCellMouseOut cell)
        ]


clickedView : Cell -> Html Msg
clickedView cell =
    tileView ( 0, -23 )
        [ onMouseUp (OnCellMouseUp cell)
        , onClick (OnCellMouseClick cell)
        , onMouseOut (OnCellMouseOut cell)
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
    tileView ( -16, -39 )
        [ onMouseDown (OnCellMouseDown cell) ]


flagViewSilent : Html Msg
flagViewSilent =
    tileView ( -16, -39 ) []


errorView : Html Msg
errorView =
    tileView ( -48, -39 ) []


valueView : Int -> Html Msg
valueView value =
    tileView ( -(16 * toFloat value), -23 ) []



-- decorative views


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



-- view helpers


tileView : ( Float, Float ) -> List (Attribute Msg) -> Html Msg
tileView offset events =
    div (spriteAttributes offset 16 16 :: events ++ [ onContextMenu NoOp ]) []


spriteView : ( Float, Float ) -> Float -> Float -> Html msg
spriteView offset width height =
    div [ spriteAttributes offset width height ] []


spriteAttributes : ( Float, Float ) -> Float -> Float -> Attribute msg
spriteAttributes ( x, y ) w h =
    css
        [ backgroundImage (url "/assets/sprite.gif")
        , backgroundPosition2 (px x) (px y)
        , width (px w)
        , height (px h)
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onMouseUp (D.succeed OnMouseUp)
        , Events.onMouseDown (D.succeed OnMouseDown)
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
