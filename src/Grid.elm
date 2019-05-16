module Grid exposing
    ( Grid
    , filter
    , get
    , getMultiple
    , initialize
    , map
    , randomCoordinate
    , set
    , toList
    , toLists
    )

import Array exposing (Array)
import List.Split as List
import Random



-- source: Punie/elm-matrix


type Grid a
    = Grid
        { numRows : Int
        , numColumns : Int
        , data : Array a
        }


initialize : Int -> Int -> (( Int, Int ) -> a) -> Grid a
initialize numRows numColumns f =
    let
        f_ i =
            f (decode numColumns i)
    in
    Grid
        { numRows = numRows
        , numColumns = numColumns
        , data = Array.initialize (numRows * numColumns) f_
        }


randomCoordinate : Grid a -> (( Int, Int ) -> msg) -> Cmd msg
randomCoordinate grid message =
    Random.pair (Random.int 0 (rows grid - 1)) (Random.int 0 (columns grid - 1))
        |> Random.generate message


rows : Grid a -> Int
rows (Grid { numRows }) =
    numRows


columns : Grid a -> Int
columns (Grid { numColumns }) =
    numColumns


get : ( Int, Int ) -> Grid a -> Maybe a
get ( row, col ) (Grid { numRows, numColumns, data }) =
    if row < 0 || col < 0 || row >= numRows || col >= numColumns then
        Nothing

    else
        Array.get (encode numColumns ( row, col )) data


getMultiple : Grid a -> List ( Int, Int ) -> List a
getMultiple grid list =
    list
        |> List.map (\coord -> get coord grid)
        |> List.filterMap identity


set : ( Int, Int ) -> a -> Grid a -> Grid a
set ( row, col ) newValue (Grid { numRows, numColumns, data }) =
    let
        index =
            encode numColumns ( row, col )
    in
    Grid
        { numRows = numRows
        , numColumns = numColumns
        , data = Array.set index newValue data
        }


map : (a -> b) -> Grid a -> Grid b
map f (Grid { numRows, numColumns, data }) =
    Grid
        { numRows = numRows
        , numColumns = numColumns
        , data = Array.map f data
        }


toList : Grid a -> List a
toList (Grid { data }) =
    Array.toList data


toLists : Grid a -> List (List a)
toLists (Grid { numColumns, data }) =
    data
        |> Array.toList
        |> List.chunksOfLeft numColumns


filter : (a -> Bool) -> Grid a -> List a
filter isIncluded (Grid { data }) =
    data
        |> Array.toList
        |> List.filter isIncluded



-- utilities


encode : Int -> ( Int, Int ) -> Int
encode numColumns ( x, y ) =
    x * numColumns + y


decode : Int -> Int -> ( Int, Int )
decode numColumns index =
    let
        q =
            index // numColumns

        r =
            remainderBy numColumns index
    in
    ( q, r )
