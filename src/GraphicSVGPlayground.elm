module GraphicSVGPlayground exposing
    ( distance, collisionCircle, collisionRectangle, collisionCircleRectangle
    , repeat, wave, zigzag
    , programPicture, programAnimation, programGamebook, programGame, GameTickInfo, Subscription(..), mapSubscription
    )

{-| A beginner-friendly toolkit for building pictures, animations, gamebooks,
and games on top of the [`GraphicSVG` module](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/latest/GraphicSVG).

Start with [`programPicture`](#programPicture) to draw a static image, then
grow into [`programAnimation`](#programAnimation) or [`programGame`](#programGame)
as your project needs movement and interaction. Use the helpers in
[Point Helpers](#point-helpers) and [Animation Helpers](#animation-helpers)
along the way.


# Point Helpers

@docs distance, collisionCircle, collisionRectangle, collisionCircleRectangle


# Animation Helpers

@docs repeat, wave, zigzag


# Programs

@docs programPicture, programAnimation, programGamebook, programGame, GameTickInfo, Subscription, mapSubscription

-}

import Browser.Events
import GraphicSVG
import GraphicSVG.EllieApp



-- POINT HELPERS


{-| Find the distance between two points.

    distance ( 0, 0 ) ( 3, 4 ) == 5

This is handy for things like detecting collisions:

    isTouching =
        distance shipPosition enemyPosition < 20

-}
distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( firstX, firstY ) ( secondX, secondY ) =
    sqrt ((secondX - firstX) ^ 2 + (secondY - firstY) ^ 2)


distance1D : Float -> Float -> Float
distance1D a b =
    abs (a - b)


{-| Check whether two circles overlap, given their center points and radii.

    collisionCircle
        { center = ( 1, 1 ), radius = 3 }
        { center = ( 8, 2 ), radius = 5 }

This is handy for things like detecting when a ship and an enemy touch:

    isTouching =
        collisionCircle
            { center = shipPosition, radius = shipRadius }
            { center = enemyPosition, radius = enemyRadius }

Note: You can use radius=0 to test whether a point is inside a circle.

-}
collisionCircle :
    { center : ( Float, Float ), radius : Float }
    -> { center : ( Float, Float ), radius : Float }
    -> Bool
collisionCircle a b =
    distance a.center b.center <= a.radius + b.radius


{-| Check whether two axis-aligned rectangles overlap, given their center
points, widths, and heights.

    collisionRectangle
        { center = ( 1, 1 ), width = 7, height = 5 }
        { center = ( 8, 2 ), width = 10, height = 10 }

This is handy for things like detecting when a player and a wall touch:

    isTouching =
        collisionRectangle
            { center = playerPosition, width = playerWidth, height = playerHeight }
            { center = wallPosition, width = wallWidth, height = wallHeight }

Note: You can use width=0, height=0 to test whether a point is inside a rectangle.

-}
collisionRectangle :
    { center : ( Float, Float ), width : Float, height : Float }
    -> { center : ( Float, Float ), width : Float, height : Float }
    -> Bool
collisionRectangle a b =
    let
        ( aX, aY ) =
            a.center

        ( bX, bY ) =
            b.center

        xOverlap =
            distance1D aX bX <= a.width / 2 + b.width / 2

        yOverlap =
            distance1D aY bY <= a.height / 2 + b.height / 2
    in
    xOverlap && yOverlap


{-| Check whether a circle and a rectangle overlap.

    collisionCircleRectangle
        { center = ( 1, 1 ), radius = 3 }
        { center = ( 8, 2 ), width = 10, height = 10 }

This is handy for things like detecting when a ball and a wall touch:

    isTouching =
        collisionCircleRectangle
            { center = ballPosition, radius = ballRadius }
            { center = wallPosition, width = wallWidth, height = wallHeight }

-}
collisionCircleRectangle :
    { center : ( Float, Float ), radius : Float }
    -> { center : ( Float, Float ), width : Float, height : Float }
    -> Bool
collisionCircleRectangle circle rect =
    let
        ( circleX, circleY ) =
            circle.center

        ( rectX, rectY ) =
            rect.center

        closestXInRect =
            circleX
                |> clamp (rectX - rect.width / 2) (rectX + rect.width / 2)

        closestYInRect =
            circleY
                |> clamp (rectY - rect.height / 2) (rectY + rect.height / 2)
    in
    distance circle.center ( closestXInRect, closestYInRect ) <= circle.radius



-- ANIMATION HELPERS


{-| Ramp up from one number to another, then jump back and repeat.

Here is an animation with a rectangle that grows and then suddenly resets:

    view time =
        [ rectangle red 10 (repeat 20 80 4 time)
        ]

The height ramps from 20 to 80 over four seconds, then drops back to 20 and
starts over.

The last parameter must be the `time` parameter from [`programAnimation`](#programAnimation),
or the `timestamp` field from [`programGame`](#programGame)'s `tick` function.

-}
repeat : Float -> Float -> Float -> Float -> Float
repeat lo hi period time =
    lo + (hi - lo) * toFrac period time


{-| Smoothly wave between two numbers.

Here is an animation with a circle that resizes:

    view time =
        [ circle lightBlue (wave 50 90 7 time)
        ]

The radius of the circle will cycle between 50 and 90 every seven seconds.
It kind of looks like it is breathing.

The last parameter must be the `time` parameter from [`programAnimation`](#programAnimation),
or the `timestamp` field from [`programGame`](#programGame)'s `tick` function.

-}
wave : Float -> Float -> Float -> Float -> Float
wave lo hi period time =
    lo + (hi - lo) * (1 + cos (turns (toFrac period time))) / 2


{-| Zig zag between two numbers.

Here is an animation with a rectangle that tips back and forth:

    view time =
        [ rectangle lightGreen 20 100
            |> rotate (zigzag -20 20 4 time)
        ]

It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds. Similar to `wave`, but feels sharper and less organic.

The last parameter must be the `time` parameter from [`programAnimation`](#programAnimation),
or the `timestamp` field from [`programGame`](#programGame)'s `tick` function.

-}
zigzag : Float -> Float -> Float -> Float -> Float
zigzag lo hi period time =
    lo + (hi - lo) * abs (2 * toFrac period time - 1)


toFrac : Float -> Float -> Float
toFrac period time =
    fRemainderBy period time / period


fRemainderBy : Float -> Float -> Float
fRemainderBy modulus value =
    if abs value > modulus then
        fRemainderBy modulus ((abs value - modulus) * signum value)

    else
        value


signum : Float -> Float
signum x =
    x / abs x



-- PROGRAM / PICTURE


type alias ProgramPicture =
    ProgramGame AnimationModel AnimationMsg


{-| Make a picture. Use this as the `main =` definition of your program.
Accepts a list of shapes as its only argument.

    main =
        picture
            [ triangle 10
                |> filled black
            , circle 2
                |> filled red
            ]

-}
programPicture :
    List (GraphicSVG.Shape Never)
    -> ProgramPicture
programPicture shapes =
    programAnimation (always shapes)



-- PROGRAM / ANIMATION


type alias AnimationModel =
    Float


type alias AnimationMsg =
    Never


type alias ProgramAnimation =
    ProgramGame AnimationModel AnimationMsg


{-| Make an animation. Use this as the `main =` definition of your program.

To create an animation, you must also create your own definition (usually called `view`).
This definition has a single parameter, usually called `time`, which is the current
time in seconds since the animation started. You can use it in functions
`repeat`, `zigzag`, or `wave` to actually animate your picture.

    main =
        animation view

    view time =
        [ square (repeat 0 10 2 time)
            |> filled red
        ]

-}
programAnimation :
    (Float -> List (GraphicSVG.Shape Never))
    -> ProgramAnimation
programAnimation viewFunction =
    let
        init : AnimationModel
        init =
            0

        update : AnimationMsg -> AnimationModel -> AnimationModel
        update msg _ =
            never msg

        view : AnimationModel -> List (GraphicSVG.Shape AnimationMsg)
        view model =
            viewFunction model

        tick : GameTickInfo -> AnimationModel -> AnimationModel
        tick info _ =
            info.timestamp
    in
    programGame
        { init = init
        , update = update
        , tick = tick
        , view = view
        , subscriptions = []
        }



-- PROGRAM / GAMEBOOK


type alias GamebookModel userModel =
    { userModel : userModel
    , screenSize : ( Float, Float )
    }


type GamebookMsg userModel
    = GamebookUserInteraction userModel


{-| Make a "gamebook" - a choose-your-own-adventure style program where the
user clicks buttons to move between states, instead of using the mouse or
keyboard in real time.

    type State = Start | End

    main =
        programGamebook
            { init = Start
            , transitions = transitions
            , view = view
            }

    transitions state =
        if state == Start then
            [ { name = "Continue", target = End }
            ]
        else
            []

    view state =
        [ ...
        ]

where:

  - `init` - the starting state.

  - `transitions` - given the current state, list the available choices
    (each with a `name` and the `target` state to jump to when clicked).

  - `view` - draw the current state (same as you would a picture, i.e. list of shapes).

-}
programGamebook :
    { init : userModel
    , transitions : userModel -> List { name : String, target : userModel }
    , view : userModel -> List (GraphicSVG.Shape Never)
    }
    ->
        ProgramGame
            (GamebookModel userModel)
            (GamebookMsg userModel)
programGamebook config =
    let
        init : GamebookModel userModel
        init =
            { screenSize = ( 0, 0 )
            , userModel = config.init
            }

        update : GamebookMsg userModel -> GamebookModel userModel -> GamebookModel userModel
        update msg model =
            case msg of
                GamebookUserInteraction userModel ->
                    { model | userModel = userModel }

        optionHeight =
            8

        viewOption : Int -> { name : String, target : userModel } -> GraphicSVG.Shape userModel
        viewOption index opt =
            GraphicSVG.text opt.name
                |> GraphicSVG.size 5
                |> GraphicSVG.centered
                |> GraphicSVG.filled GraphicSVG.black
                |> GraphicSVG.notifyTap opt.target
                |> GraphicSVG.move ( 0, toFloat (index * optionHeight) )

        view : GamebookModel userModel -> List (GraphicSVG.Shape (GamebookMsg userModel))
        view model =
            let
                transitions =
                    config.transitions model.userModel

                transitionsHeight =
                    toFloat (List.length transitions) * optionHeight

                screenHeight =
                    Tuple.second model.screenSize
            in
            [ config.view model.userModel
                |> GraphicSVG.group
                |> GraphicSVG.map never
                |> GraphicSVG.move ( 0, transitionsHeight / 2 )
            , GraphicSVG.square 500
                |> GraphicSVG.filled GraphicSVG.lightGray
                |> GraphicSVG.move ( 0, -250 - screenHeight / 2 + transitionsHeight )
            , transitions
                |> List.reverse
                |> List.indexedMap viewOption
                |> GraphicSVG.group
                |> GraphicSVG.map GamebookUserInteraction
                |> GraphicSVG.move ( 0, screenHeight / -2 + 3 )
            ]

        tick : GameTickInfo -> GamebookModel userModel -> GamebookModel userModel
        tick info model =
            { model | screenSize = ( info.screenWidth, info.screenHeight ) }
    in
    programGame
        { init = init
        , update = update
        , tick = tick
        , view = view
        , subscriptions = []
        }



-- PROGRAM / GAME


type alias Flags =
    ( Int, Int )


type alias GameModel userModel =
    { timestamp : Float
    , userModel : userModel
    , screenSize : Flags
    }


type GameMsg userMsg
    = GameResize Int Int
    | GameTick Float GraphicSVG.EllieApp.GetKeyState
    | GameUserInteraction userMsg


{-| The information passed to a [`programGame`](#programGame)'s `tick` function
on every animation frame.

  - `delta` - seconds elapsed since the previous frame.
  - `timestamp` - seconds elapsed since the program started.
  - `isKeyPressed` - whether a given key (e.g. `"LeftArrow"`, `"Shift"`) is
    currently held down.
  - `screenWidth`, `screenHeight` - the current size of the visible screen.

-}
type alias GameTickInfo =
    { delta : Float
    , timestamp : Float
    , isKeyPressed : String -> Bool
    , screenWidth : Float
    , screenHeight : Float
    }


type alias ProgramGame userModel userMsg =
    GraphicSVG.EllieApp.EllieAppWithTick
        Flags
        (GameModel userModel)
        (GameMsg userMsg)


{-| Make a game! This is the most flexible way to build a program: you get a
model that updates every frame, keyboard input, and screen size.

    main =
        programGame
            { init = init
            , tick = tick
            , update = update
            , view = view
            , subscriptions = []
            }

    init =
        { x = 0 }

    tick info model =
        { model | x = model.x + info.delta }

    update () model =
        model

    view model =
        [ circle 5
            |> filled red
            |> move ( model.x, 0 )
        ]

where:

  - `init` - the starting model.

  - `tick` - called on every animation frame with a [`GameTickInfo`](#GameTickInfo)
    (delta time, timestamp, key state, screen size), so you can move things
    and react to held-down keys.

  - `update` - called in response to `subscriptions` events (e.g. a key being
    pressed once).

  - `view` - draw the model's shapes.

  - `subscriptions` - a list of one-off key events (see [`Subscription`](#Subscription))
    that trigger `update`.

-}
programGame :
    { init : userModel
    , update : userMsg -> userModel -> userModel
    , tick : GameTickInfo -> userModel -> userModel
    , view : userModel -> List (GraphicSVG.Shape userMsg)
    , subscriptions : List (Subscription userMsg)
    }
    -> ProgramGame userModel userMsg
programGame config =
    let
        init : Flags -> ( GameModel userModel, Cmd (GameMsg userMsg) )
        init flags =
            ( { timestamp = 0
              , screenSize = flags
              , userModel = config.init
              }
            , Cmd.none
            )

        update : GameMsg userMsg -> GameModel userModel -> ( GameModel userModel, Cmd (GameMsg userMsg) )
        update msg model =
            case msg of
                GameTick timestamp ( getKeyState, _, _ ) ->
                    let
                        userMessages : List userMsg
                        userMessages =
                            config.subscriptions |> List.filterMap (subToMsg getKeyState)

                        updates =
                            userMessages |> List.map config.update

                        ( screenWidth, screenHeight ) =
                            screenPixelsToSize model.screenSize

                        tickInfo : GameTickInfo
                        tickInfo =
                            { delta =
                                timestamp - model.timestamp
                            , timestamp =
                                timestamp
                            , isKeyPressed =
                                \key ->
                                    getKeyState (stringToKey key) == GraphicSVG.EllieApp.Down
                            , screenHeight = screenHeight
                            , screenWidth = screenWidth
                            }
                    in
                    ( { model
                        | timestamp = timestamp
                        , userModel =
                            List.foldl (<|) model.userModel updates
                                |> config.tick tickInfo
                      }
                    , Cmd.none
                    )

                GameUserInteraction userMsg ->
                    ( { model
                        | userModel = config.update userMsg model.userModel
                      }
                    , Cmd.none
                    )

                GameResize x y ->
                    ( { model | screenSize = ( x, y ) }
                    , Cmd.none
                    )

        view : GameModel userModel -> { title : String, body : GraphicSVG.Collage (GameMsg userMsg) }
        view model =
            { title = "Game"
            , body =
                config.view model.userModel
                    |> renderNormalized model.screenSize
                    |> GraphicSVG.mapCollage GameUserInteraction
            }

        subscriptions : GameModel userModel -> Sub (GameMsg userMsg)
        subscriptions _ =
            Browser.Events.onResize GameResize

        tickMsg : GraphicSVG.EllieApp.InputHandler (GameMsg userMsg)
        tickMsg =
            GameTick
    in
    GraphicSVG.EllieApp.ellieAppWithTick
        tickMsg
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


{-| Describes a one-off keyboard event to react to in [`programGame`](#programGame).

Unlike `GameTickInfo.isKeyPressed` (which reports keys held down every frame),
a `Subscription` fires `update` with the given message exactly once, when the
key is pressed or released.

    subscriptions =
        [ OnKeyDown "Space" Jump
        , OnKeyUp "c" ToggleColor
        ]

Note: `Jump` and `ToggleColor` are messages defined in your program.
You can use any type as a message.

Possible keys: `"Alt"`, `"Backspace"`, `"Caps"`, `"Ctrl"`, `"Delete"`,
`"DownArrow"`, `"Enter"`, `"LeftArrow"`, `"RightArrow"`, `"Shift"`, `"Space"`,
`"Tab"`, `"UpArrow"`, `"a"` - `"z"`, `"0"` - `"9"`

-}
type Subscription userMsg
    = OnKeyDown String userMsg
    | OnKeyUp String userMsg


{-| **(Advanced function)** Transform the message produced by a [`Subscription`](#Subscription).
Useful when combining subscriptions from a sub-module into a bigger program.
-}
mapSubscription : (a -> b) -> Subscription a -> Subscription b
mapSubscription transformMsg subA =
    case subA of
        OnKeyDown event msgA ->
            OnKeyDown event (transformMsg msgA)

        OnKeyUp event msgA ->
            OnKeyUp event (transformMsg msgA)


subToMsg : (GraphicSVG.EllieApp.Keys -> GraphicSVG.EllieApp.KeyState) -> Subscription userMsg -> Maybe userMsg
subToMsg getKeyState sub =
    let
        detectKey key position msg =
            if getKeyState key == position then
                Just msg

            else
                Nothing
    in
    case sub of
        OnKeyDown key msg ->
            detectKey (stringToKey key) GraphicSVG.EllieApp.JustDown msg

        OnKeyUp key msg ->
            detectKey (stringToKey key) GraphicSVG.EllieApp.JustUp msg



-- SHARED


screenPixelsToSize : ( Int, Int ) -> ( Float, Float )
screenPixelsToSize ( x, y ) =
    let
        coefficient =
            100 / sqrt (toFloat (x * y))

        normalize : Int -> Float
        normalize n =
            toFloat n * coefficient
    in
    ( normalize x
    , normalize y
    )


renderNormalized : ( Int, Int ) -> List (GraphicSVG.Shape msg) -> GraphicSVG.Collage msg
renderNormalized screen shapes =
    let
        ( w, h ) =
            screenPixelsToSize screen
    in
    GraphicSVG.collage w h shapes


stringToKey : String -> GraphicSVG.EllieApp.Keys
stringToKey strKey =
    case strKey of
        "Alt" ->
            GraphicSVG.EllieApp.Alt

        "Backspace" ->
            GraphicSVG.EllieApp.Backspace

        "Caps" ->
            GraphicSVG.EllieApp.Caps

        "Ctrl" ->
            GraphicSVG.EllieApp.Ctrl

        "Delete" ->
            GraphicSVG.EllieApp.Delete

        "DownArrow" ->
            GraphicSVG.EllieApp.DownArrow

        "Enter" ->
            GraphicSVG.EllieApp.Enter

        "LeftArrow" ->
            GraphicSVG.EllieApp.LeftArrow

        "RightArrow" ->
            GraphicSVG.EllieApp.RightArrow

        "Shift" ->
            GraphicSVG.EllieApp.Shift

        "Space" ->
            GraphicSVG.EllieApp.Space

        "Tab" ->
            GraphicSVG.EllieApp.Tab

        "UpArrow" ->
            GraphicSVG.EllieApp.UpArrow

        key ->
            GraphicSVG.EllieApp.Key key
