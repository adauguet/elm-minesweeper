module Grid exposing
    ( Grid
    , columns
    , filter
    , get
    , getMultiple
    , indexedMap
    , initialize
    , map
    , randomCoordinate
    , randomCoordinates
    , rows
    , set
    )

import Array exposing (Array)
import Random
import Random.List
import Set exposing (Set)


type alias Grid a =
    Array (Array a)


initialize : Int -> Int -> (( Int, Int ) -> a) -> Grid a
initialize numRows numColumns f =
    Array.initialize numRows
        (\row ->
            Array.initialize
                numColumns
                (\col -> f ( row, col ))
        )


randomCoordinate : Grid a -> (( Int, Int ) -> msg) -> Cmd msg
randomCoordinate grid message =
    Random.pair (Random.int 0 (rows grid - 1)) (Random.int 0 (columns grid - 1))
        |> Random.generate message


randomCoordinates : Grid a -> Int -> (List ( Int, Int ) -> msg) -> Cmd msg
randomCoordinates grid count message =
    grid
        |> indexedMap (\( x, y ) _ -> ( x, y ))
        |> filter (\_ -> True)
        |> Random.List.shuffle
        |> Random.map (List.take count)
        |> Random.generate message


rows : Grid a -> Int
rows grid =
    Array.length grid


columns : Grid a -> Int
columns grid =
    grid
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


get : ( Int, Int ) -> Grid a -> Maybe a
get ( row, col ) grid =
    grid
        |> Array.get row
        |> Maybe.andThen (Array.get col)


getMultiple : Grid a -> List ( Int, Int ) -> List a
getMultiple grid list =
    list
        |> List.map (\coord -> get coord grid)
        |> List.filterMap identity


set : ( Int, Int ) -> a -> Grid a -> Grid a
set ( row, col ) newValue grid =
    grid
        |> Array.get row
        |> Maybe.map (\rowAry -> Array.set row (Array.set col newValue rowAry) grid)
        |> Maybe.withDefault grid


indexedMap : (( Int, Int ) -> a -> b) -> Grid a -> Grid b
indexedMap f grid =
    grid
        |> Array.indexedMap
            (\row rowAry ->
                Array.indexedMap
                    (\col value -> f ( row, col ) value)
                    rowAry
            )


map : (a -> b) -> Grid a -> Grid b
map f grid =
    indexedMap (\_ value -> f value) grid


filter : (a -> Bool) -> Grid a -> List a
filter isIncluded grid =
    grid
        |> Array.map (\array -> Array.filter isIncluded array |> Array.toList)
        |> Array.toList
        |> List.concat
