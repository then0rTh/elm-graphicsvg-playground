# elm-graphicsvg-playground

A friendly, `main =`-oriented API for drawing pictures, animations, gamebooks,
and games with [`MacCASOutreach/graphicsvg`](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/latest/).

The actual shapes, colors and transformations are all from then [`GraphicSVG` module](https://package.elm-lang.org/packages/MacCASOutreach/graphicsvg/latest/GraphicSVG)


```elm
import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..)

main =
    programPicture
        [ triangle 10
            |> filled black
        , circle 2
            |> filled red
        ]
```


## Usage

### A picture

The simplest program is a static picture — a list of GraphicSVG shapes.

```elm
import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..)

main =
    programPicture
        [ triangle 10
            |> filled black
        , circle 2
            |> filled red
        ]
```

### An animation

Add motion by switching to `programAnimation` and writing a `view` function
that takes the elapsed `time` (in seconds). Combine it with `repeat`, `wave`,
or `zigzag` to animate shapes without hand-rolling the math.

```elm
import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..

main =
    programAnimation view

view time =
    [ circle (wave 50 90 7 time)
        |> filled lightBlue
    ]
```

### A gamebook

Build a choose-your-own-adventure style program where users click buttons to
move between states.

```elm
import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..)

type State
    = Start
    | End

main =
    programGamebook
        { init = Start
        , transitions = transitions
        , view = view
        }

transitions state =
    if state == Start then
        [ { name = "Continue", target = End } ]

    else
        []

view state =
    [ text "Hello!"
        |> filled black
    ]
```

### A game

For full interactivity — keyboard input, per-frame updates, and screen size —
use `programGame`.

```elm
import GraphicSVG exposing (..)
import GraphicSVGPlayground exposing (..)

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
```

See the [`GraphicSVGPlayground`](https://package.elm-lang.org/packages/then0rTh/elm-graphicsvg-playground/latest/GraphicSVGPlayground)
module documentation for the full API reference, including `Subscription` for
one-off key events and `GameTickInfo` for the data available on every frame.


## Installation

```sh
elm install MacCASOutreach/graphicsvg then0rTh/elm-graphicsvg-playground
```
