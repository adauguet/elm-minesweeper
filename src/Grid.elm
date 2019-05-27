module Grid exposing
    ( Grid
    , filter
    , get
    , getMultiple
    , initialize
    , map
    , randomCoordinates
    , set
    , toList
    , toLists
    )

import Array exposing (Array)
import List.Split as List
import Random exposing (Generator)
import Random.Array.Extra
import Set


{-| source: Punie/elm-matrix
The Grid's internal implementation is hidden by not exposing the constructor.
-}
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


get : ( Int, Int ) -> Grid a -> Maybe a
get ( row, col ) (Grid { numRows, numColumns, data }) =
    encode numRows numColumns ( row, col )
        |> Maybe.andThen (\index -> Array.get index data)


getMultiple : Grid a -> List ( Int, Int ) -> List a
getMultiple grid list =
    list
        |> List.map (\coord -> get coord grid)
        |> List.filterMap identity


set : ( Int, Int ) -> a -> Grid a -> Grid a
set ( row, col ) newValue (Grid { numRows, numColumns, data }) =
    case encode numRows numColumns ( row, col ) of
        Just index ->
            Grid
                { numRows = numRows
                , numColumns = numColumns
                , data = Array.set index newValue data
                }

        Nothing ->
            Grid
                { numRows = numRows
                , numColumns = numColumns
                , data = data
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


randomCoordinates : Int -> Grid a -> List ( Int, Int ) -> Generator (List ( Int, Int ))
randomCoordinates count (Grid grid) avoid =
    let
        avoid_ =
            avoid
                |> List.map (encode grid.numRows grid.numColumns)
                |> List.filterMap identity
                |> Set.fromList
    in
    -- compute all coordinates
    (Array.length grid.data - 1)
        |> List.range 0
        -- convert to set
        |> Set.fromList
        -- remove coordinates to avoid
        |> (\s -> Set.diff s avoid_)
        -- convert to List
        |> Set.toList
        -- convert to Array
        |> Array.fromList
        -- shuffle Array
        |> Random.Array.Extra.shuffle
        -- convert to List, take count, decode
        |> Random.map (Array.toList >> List.take count >> List.map (decode grid.numColumns))



-- utilities


encode : Int -> Int -> ( Int, Int ) -> Maybe Int
encode numRows numColumns ( x, y ) =
    if x < 0 || y < 0 || x >= numRows || y >= numColumns then
        Nothing

    else
        Just (x * numColumns + y)


decode : Int -> Int -> ( Int, Int )
decode numColumns index =
    let
        q =
            index // numColumns

        r =
            remainderBy numColumns index
    in
    ( q, r )
