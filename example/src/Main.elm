module Main exposing (main)

import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..)


main =
    programAnimationHelp view


view time =
    group
        [ square 5
            |> filled black
            |> move ( wave -20 20 5 time, wave -20 20 10 time )
            |> move ( 10, 0 )
        , square 5
            |> filled black
            |> move ( zigzag -20 20 5 time, zigzag -20 20 10 time )
            |> move ( -20, 0 )
        ]
