module Cell exposing (Cell, CellState(..), CellValue(..))


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
