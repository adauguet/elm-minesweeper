module Grid exposing
    ( Grid
    , allCoordinates
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
import Random.Array



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


randomCoordinates : Int -> Grid a -> Generator (List ( Int, Int ))
randomCoordinates count (Grid grid) =
    List.range 0 (Array.length grid.data - 1)
        |> Array.fromList
        |> shuffle
        |> Random.map (Array.toList >> List.take count >> List.map (decode grid.numColumns))


{-| source elm-community/random-extra on release 3.0.0 with a real Fisher Yates algorithm
-}
shuffle : Array a -> Generator (Array a)
shuffle arr =
    if Array.isEmpty arr then
        Random.constant arr

    else
        let
            helper : ( List a, Array a ) -> Generator ( List a, Array a )
            helper ( done, remaining ) =
                Random.Array.choose remaining
                    |> Random.andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    Random.constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
        Random.map (Tuple.first >> Array.fromList) (helper ( [], arr ))


allCoordinates : Grid a -> List ( Int, Int )
allCoordinates (Grid { numColumns, data }) =
    List.range 0 (Array.length data - 1)
        |> List.map (decode numColumns)



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
