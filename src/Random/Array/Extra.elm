module Random.Array.Extra exposing (shuffle)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array


{-| source: elm-community/random-extra
The implementation in the current version 3.1.0 does not provide satisfying results.
This implementation is extracted from version 3.0.0 and uses Fisher Yates algorithm.
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
