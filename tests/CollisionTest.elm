module CollisionTest exposing (circleRectangleSuite, circleSuite, rectangleSuite)

import Expect
import Fuzz exposing (Fuzzer)
import GraphicSVGPlayground exposing (collisionCircle, collisionCircleRectangle, collisionRectangle)
import Test exposing (Test, describe, test)


circleSuite : Test
circleSuite =
    describe "collisionCircle"
        [ test "overlapping circles collide" <|
            \_ ->
                collisionCircle
                    { center = ( 0, 0 ), radius = 5 }
                    { center = ( 8, 0 ), radius = 5 }
                    |> Expect.equal True
        , test "distant circles do not collide" <|
            \_ ->
                collisionCircle
                    { center = ( 0, 0 ), radius = 5 }
                    { center = ( 20, 0 ), radius = 5 }
                    |> Expect.equal False
        , test "circles exactly touching collide" <|
            \_ ->
                collisionCircle
                    { center = ( 0, 0 ), radius = 5 }
                    { center = ( 10, 0 ), radius = 5 }
                    |> Expect.equal True
        , test "one circle inside another collides" <|
            \_ ->
                collisionCircle
                    { center = ( 0, 0 ), radius = 10 }
                    { center = ( 1, 1 ), radius = 2 }
                    |> Expect.equal True
        , test "point collides (r=0)" <|
            \_ ->
                collisionCircle
                    { center = ( 0, 0 ), radius = 10 }
                    { center = ( -5, 5 ), radius = 0 }
                    |> Expect.equal True
        , Test.fuzz circleFuzzer "identical circles always collide" <|
            \a ->
                collisionCircle a a
                    |> Expect.equal True
        , Test.fuzz2 circleFuzzer circleFuzzer "collision is symmetric" <|
            \a b ->
                collisionCircle a b
                    |> Expect.equal (collisionCircle b a)
        ]


rectangleSuite : Test
rectangleSuite =
    describe "collisionRectangle"
        [ test "overlapping rectangles collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( -8, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , test "distant rectangles do not collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( 20, 0 ), width = 10, height = 10 }
                    |> Expect.equal False
        , test "rectangles overlapping only on both axes collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( 5, 5 ), width = 10, height = 10 }
                    |> Expect.equal True
        , test "rectangles overlapping only on the x-axis do not collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( 5, 20 ), width = 10, height = 10 }
                    |> Expect.equal False
        , test "rectangles overlapping only on the y-axis do not collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( 20, 5 ), width = 10, height = 10 }
                    |> Expect.equal False
        , test "rectangles exactly touching edges collide" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    { center = ( 10, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , test "one rectangle fully inside another collides" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 20, height = 20 }
                    { center = ( 1, 1 ), width = 2, height = 2 }
                    |> Expect.equal True
        , test "point collides (w=0,h=0)" <|
            \_ ->
                collisionRectangle
                    { center = ( 0, 0 ), width = 20, height = 20 }
                    { center = ( 10, -10 ), width = 0, height = 0 }
                    |> Expect.equal True
        , Test.fuzz rectangleFuzzer "identical rectangles always collide" <|
            \a ->
                collisionRectangle a a
                    |> Expect.equal True
        , Test.fuzz2 rectangleFuzzer rectangleFuzzer "collision is symmetric" <|
            \a b ->
                collisionRectangle a b
                    |> Expect.equal (collisionRectangle b a)
        ]


circleRectangleSuite : Test
circleRectangleSuite =
    describe "collisionCircleRectangle"
        [ test "circle overlapping rectangle collides" <|
            \_ ->
                collisionCircleRectangle
                    { center = ( 0, 0 ), radius = 5 }
                    { center = ( 8, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , test "distant circle and rectangle do not collide" <|
            \_ ->
                collisionCircleRectangle
                    { center = ( 0, 0 ), radius = 5 }
                    { center = ( 30, 0 ), width = 10, height = 10 }
                    |> Expect.equal False
        , test "circle near rectangle corner but out of reach does not collide" <|
            \_ ->
                collisionCircleRectangle
                    { center = ( 6, 6 ), radius = sqrt 2 - 0.01 }
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    |> Expect.equal False
        , test "circle just reaching rectangle corner collides" <|
            \_ ->
                collisionCircleRectangle
                    { center = ( 6, 6 ), radius = sqrt 2 + 0.01 }
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , test "circle touching flat edge of rectangle collides" <|
            \_ ->
                collisionCircleRectangle
                    { center = ( 10, 0 ), radius = 5 }
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , Test.fuzz (Fuzz.oneOfValues [ ( -1, 1 ), ( 5, 1 ), ( 5, -5 ) ]) "point (r=0) collides with rectangle" <|
            \c ->
                collisionCircleRectangle
                    { center = c, radius = 0 }
                    { center = ( 0, 0 ), width = 10, height = 10 }
                    |> Expect.equal True
        , Test.fuzz (Fuzz.oneOfValues [ 5, 5.1, 6 ]) "circle collides with point (w=0,h=0)" <|
            \r ->
                collisionCircleRectangle
                    { center = ( 3, 4 ), radius = r }
                    { center = ( 0, 0 ), width = 0, height = 0 }
                    |> Expect.equal True
        , Test.fuzz2 radiusFuzzer rectangleFuzzer "a circle centered at the rectangle's center always collides" <|
            \r rect ->
                collisionCircleRectangle { center = rect.center, radius = r } rect
                    |> Expect.equal True
        , Test.fuzz2 circleFuzzer rectangleFuzzer "is symmetric (swap center)" <|
            \circle rect ->
                collisionCircleRectangle circle rect
                    |> Expect.equal
                        (collisionCircleRectangle
                            { circle | center = rect.center }
                            { rect | center = circle.center }
                        )
        ]


pointFuzzer : Fuzzer ( Float, Float )
pointFuzzer =
    Fuzz.pair
        (Fuzz.floatRange -1000 1000)
        (Fuzz.floatRange -1000 1000)


radiusFuzzer : Fuzzer Float
radiusFuzzer =
    Fuzz.floatRange 0 1000


circleFuzzer : Fuzzer { center : ( Float, Float ), radius : Float }
circleFuzzer =
    Fuzz.map2 (\center radius -> { center = center, radius = radius })
        pointFuzzer
        radiusFuzzer


dimensionFuzzer : Fuzzer Float
dimensionFuzzer =
    Fuzz.floatRange 0 1000


rectangleFuzzer : Fuzzer { center : ( Float, Float ), width : Float, height : Float }
rectangleFuzzer =
    Fuzz.map3 (\center width height -> { center = center, width = width, height = height })
        pointFuzzer
        dimensionFuzzer
        dimensionFuzzer
