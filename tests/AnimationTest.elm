module AnimationTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import GraphicSVGPlayground
import Test exposing (Test)


suite : Test
suite =
    Test.describe "animation functions"
        [ repeatSuite
        , circularSuite "wave" GraphicSVGPlayground.wave
        , circularSuite "zigzag" GraphicSVGPlayground.zigzag
        ]


repeatSuite : Test
repeatSuite =
    Test.describe "repeat"
        [ alwaysWithinTest GraphicSVGPlayground.repeat
        , Test.fuzz2 loHiFuzzer periodFuzzer "returns half at half-period" <|
            \( lo, hi ) ( period, periodCoef ) ->
                GraphicSVGPlayground.repeat lo hi period (period * toFloat periodCoef + period / 2)
                    |> Expect.within (Expect.Absolute 0.0001) ((hi + lo) / 2)
        , Test.fuzz2 loHiFuzzer periodFuzzer "returns lo just after period" <|
            \( lo, hi ) ( period, periodCoef ) ->
                GraphicSVGPlayground.repeat lo hi period (period * toFloat periodCoef + period * 0.01)
                    |> Expect.within (Expect.Absolute (abs (hi - lo) * 0.01 + 0.0001)) lo
        , Test.fuzz2 loHiFuzzer periodFuzzer "returns hi just before period" <|
            \( lo, hi ) ( period, periodCoef ) ->
                GraphicSVGPlayground.repeat lo hi period (period * toFloat periodCoef + period * 0.99)
                    |> Expect.within (Expect.Absolute (abs (hi - lo) * 0.99 + 0.0001)) hi
        ]


circularSuite : String -> (Float -> Float -> Float -> Float -> Float) -> Test
circularSuite name fn =
    Test.describe name
        [ alwaysWithinTest fn
        , Test.fuzz2 loHiFuzzer periodFuzzer "returns hi at period" <|
            \( lo, hi ) ( period, periodCoef ) ->
                fn lo hi period (period * toFloat periodCoef)
                    |> Expect.within (Expect.Absolute 0.0001) hi
        , Test.fuzz2 loHiFuzzer periodFuzzer "returns lo halfway through the period" <|
            \( lo, hi ) ( period, periodCoef ) ->
                fn lo hi period (period * toFloat periodCoef + period / 2)
                    |> Expect.within (Expect.Absolute 0.0001) lo
        ]


alwaysWithinTest : (Float -> Float -> Float -> Float -> Float) -> Test
alwaysWithinTest fn =
    Test.fuzz3 loHiFuzzer periodFuzzer timestampFuzzer "is always within lo and hi" <|
        \( lo, hi ) ( period, _ ) timestamp ->
            fn lo hi period timestamp
                |> Expect.within (Expect.Absolute (abs (hi - lo) / 2 + 0.0001)) ((lo + hi) / 2)


loHiFuzzer : Fuzzer ( Float, Float )
loHiFuzzer =
    Fuzz.pair
        (Fuzz.floatRange -1000 1000)
        (Fuzz.floatRange -1000 1000)


periodFuzzer : Fuzzer ( Float, Int )
periodFuzzer =
    Fuzz.pair
        (Fuzz.floatRange 0.1 (60 * 60))
        (Fuzz.intRange 0 24)


timestampFuzzer : Fuzzer Float
timestampFuzzer =
    Fuzz.floatRange 0 (60 * 60 * 24)
