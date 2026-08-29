module DistanceTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import GraphicSVGPlayground exposing (distance)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "distance"
        [ Test.fuzz pointFuzzer "distance between the same point is 0" <|
            \point ->
                distance point point
                    |> Expect.within (Expect.Absolute 0.0001) 0
        , test "classic 3-4-5 triangle" <|
            \_ ->
                distance ( 0, 0 ) ( 3, 4 )
                    |> Expect.within (Expect.Absolute 0.0001) 5
        , Test.fuzz2 pointFuzzer pointFuzzer "is symmetric" <|
            \a b ->
                distance a b
                    |> Expect.within (Expect.Absolute 0.0001) (distance b a)
        , test "handles negative coordinates" <|
            \_ ->
                distance ( -3, -4 ) ( 0, 0 )
                    |> Expect.within (Expect.Absolute 0.0001) 5
        , test "horizontal distance" <|
            \_ ->
                distance ( 2, 5 ) ( 7, 5 )
                    |> Expect.within (Expect.Absolute 0.0001) 5
        , test "vertical distance" <|
            \_ ->
                distance ( 2, 5 ) ( 2, -1 )
                    |> Expect.within (Expect.Absolute 0.0001) 6
        ]


pointFuzzer : Fuzzer ( Float, Float )
pointFuzzer =
    Fuzz.pair
        (Fuzz.floatRange -1000 1000)
        (Fuzz.floatRange -1000 1000)
