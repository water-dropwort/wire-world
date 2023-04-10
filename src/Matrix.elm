module Matrix exposing (..)

import Array


type alias Matrix a =
    { data : Array.Array a
    , rowLength : Int
    , columnLength : Int
    }


repeat : Int -> Int -> a -> Matrix a
repeat rowLen colLen value =
    { data = Array.repeat (rowLen * colLen) value
    , rowLength = rowLen
    , columnLength = colLen
    }


get : Int -> Int -> Matrix a -> Maybe a
get row col mat =
    if row < 0 || row >= mat.rowLength || col < 0 || col >= mat.columnLength then
        Nothing

    else
        Array.get (row * mat.columnLength + col) mat.data


set : Int -> Int -> a -> Matrix a -> Matrix a
set row col value mat =
    if row < 0 || row >= mat.rowLength || col < 0 || col >= mat.columnLength then
        mat

    else
        { mat | data = Array.set (row * mat.columnLength + col) value mat.data }


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f mat =
    { data =
        Array.indexedMap
            (\i v ->
                let
                    row =
                        i // mat.columnLength

                    col =
                        i - (i // mat.columnLength * mat.columnLength)
                in
                f row col v
            )
            mat.data
    , rowLength = mat.rowLength
    , columnLength = mat.columnLength
    }


toList : Matrix a -> List a
toList mat =
    Array.toList mat.data


fromList : Int -> Int -> List a -> Maybe (Matrix a)
fromList rowLen colLen list =
    if rowLen <= 0 || colLen <= 0 || List.length list /= (rowLen * colLen) then
        Nothing

    else
        Just
            { data = Array.fromList list
            , rowLength = rowLen
            , columnLength = colLen
            }
