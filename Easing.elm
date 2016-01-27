module Easing (ease,
 Interpolation, Animation,
 float, point2d, point3d, color, pair,
 cycle, invert, retour, inOut, flip,
 Easing,
 bezier,
 linear,
 easeInQuad, easeOutQuad, easeInOutQuad,
 easeInCubic, easeOutCubic, easeInOutCubic,
 easeInQuart, easeOutQuart, easeInOutQuart,
 easeInQuint, easeOutQuint, easeInOutQuint,
 easeInSine, easeOutSine, easeInOutSine,
 easeInExpo, easeOutExpo, easeInOutExpo,
 easeInCirc, easeOutCirc, easeInOutCirc,
 easeInBack, easeOutBack, easeInOutBack,
 easeInBounce, easeOutBounce, easeInOutBounce,
 easeInElastic, easeOutElastic, easeInOutElastic
 ) where

{-| `Easing` is a library for working creating simple animations with easing functions. Easing functions interpolate a value over time. This can be a value of any type, including numbers, points and colors.

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings").

```elm
sampleAnimation : Time -> Float
sampleAnimation currentTime =
    ease easeInCubic float 0 10 second currentTime

{- Transition from blue to red using custom `Easing` function -}
customAnimation : Time -> Color
customAnimation currentTime =
    ease (\x -> x ^ 2.4) color blue red second currentTime

{- Animate between 0 and 5 with the easeInOutQuad Easing -}
animation1 : Time -> Float
animation1 currentTime =
    ease easeInOutQuad number 0 5 second currentTime

{- Animation with bezier curve -}
bezierAnimation : Time -> Float
bezierAnimation currentTime =
    ease (bezier 0.65 0.06 0.99 0) number 0 5 second currentTime

{- Create your own Interpolation functions -}
vec : Interpolation Vec3
vec from to value =
    from `add` ((to `sub` from) `scale` value)

{- Use your Easing and Interpolation functions -}
vec3movement : Time -> Vec3
vec3movement currentTime =
    ease easeInQuad vec (vec3 0 0 0) (vec3 10 10 10) (3 * second) currentTime
```

# Easing
@docs ease

# Interpolation and Animation
@docs Interpolation, Animation

# Interpolation functions
@docs float, point2d, point3d, color, pair

#Easing function manipulation
@docs cycle, invert, retour, inOut, flip

# Easing functions
@docs Easing,
      bezier,
      linear,
      easeInQuad, easeOutQuad, easeInOutQuad,
      easeInCubic, easeOutCubic, easeInOutCubic,
      easeInQuart, easeOutQuart, easeInOutQuart,
      easeInQuint, easeOutQuint, easeInOutQuint,
      easeInSine, easeOutSine, easeInOutSine,
      easeInExpo, easeOutExpo, easeInOutExpo,
      easeInCirc, easeOutCirc, easeInOutCirc,
      easeInBack, easeOutBack, easeInOutBack,
      easeInBounce, easeOutBounce, easeInOutBounce,
      easeInElastic, easeOutElastic, easeInOutElastic
-}

import Time exposing (Time)
import Color exposing (Color, toRgb, rgba)
import List
import Maybe

{-| Type alias for Easing functions.
-}
type alias Easing = Float -> Float

{-| An interpolation of two values using a Float value.

    float : Interpolation Float
    float from to v = from + (from - to) * v
-}
type alias Interpolation a = a -> a -> Float -> a

{-| An `Animation` is a function that returns a value given a duration and the current time.
-}
type alias Animation a = Time -> Time -> a

{-| Compute an animation using the parameters.
      Parameters are: an `Easing` function, an `Interpolation` function, a `from` value, a `to` value, the duration of the transition and the elapsed time.

      ease linear float 0 20 second 0 == 0

      ease linear float 0 20 second second == 20

      ease linear color blue red second second == red
-}
ease : Easing -> Interpolation a -> a -> a -> Time -> Time -> a
ease easing interpolation from to duration time =
    interpolation from to (easing (min (time / duration) 1))

{-| Interpolation of two Floats -}
float : Interpolation Float
float from to v =
    from + (to - from) * v

{-| Interpolation of two points in 2D -}
point2d : Interpolation { x : Float, y : Float }
point2d from to v =
    { x = float from.x to.x v
    , y = float from.y to.y v
    }

{-| Interpolation of two points in 3D -}
point3d : Interpolation { x : Float, y : Float, z : Float }
point3d from to v =
    { x = float from.x to.x v
    , y = float from.y to.y v
    , z =  float from.z to.z v
    }

{-| Interpolation of two colors -}
color : Interpolation Color
color from to v =
    let
        (rgb1, rgb2)     = (toRgb from, toRgb to)
        (r1, g1, b1, a1) = (rgb1.red, rgb1.green, rgb1.blue, rgb1.alpha)
        (r2, g2, b2, a2) = (rgb2.red, rgb2.green, rgb2.blue, rgb2.alpha)
        float' from to v = round (float (toFloat from) (toFloat to) v)
    in
        rgba (float' r1 r2 v) (float' g1 g2 v) (float' b1 b2 v) (float a1 a2 v)

{-| Interpolation of a pair -}
pair : Interpolation a -> Interpolation (a, a)
pair interpolate (a0, b0) (a1, b1) v =
    (interpolate a0 a1 v, interpolate b0 b1 v)

{-|-}
linear : Easing
linear =
    identity

{-| A cubic bezier function using 4 parameters: x and y position of first control point, and x and y position of second control point.

See [here](http://greweb.me/glsl-transition/example/ "glsl-transitions") for examples or [here](http://cubic-bezier.com/ "tester") to test.
 -}
bezier : Float -> Float -> Float -> Float -> Easing
bezier x1 y1 x2 y2 time =
    let casteljau ps =
        case ps of
            [(x,y)] ->
                y
            xs ->
                List.map2 (\x y -> pair float x y time) xs (Maybe.withDefault [] (List.tail xs))
                  |> casteljau
    in
        casteljau [(0, 0), (x1, y1), (x2, y2), (1, 1)]

{-|-}
easeInQuad : Easing
easeInQuad time =
    time ^ 2

{-|-}
easeOutQuad : Easing
easeOutQuad =
    invert easeInQuad

{-|-}
easeInOutQuad : Easing
easeInOutQuad =
    inOut easeInQuad easeOutQuad

{-|-}
easeInCubic : Easing
easeInCubic time =
    time ^ 3

{-|-}
easeOutCubic : Easing
easeOutCubic =
    invert easeInCubic

{-|-}
easeInOutCubic : Easing
easeInOutCubic =
    inOut easeInCubic easeOutCubic

{-|-}
easeInQuart : Easing
easeInQuart time =
    time ^ 4

{-|-}
easeOutQuart : Easing
easeOutQuart =
    invert easeInQuart

{-|-}
easeInOutQuart : Easing
easeInOutQuart =
    inOut easeInQuart easeOutQuart

{-|-}
easeInQuint : Easing
easeInQuint time =
    time ^ 5

{-|-}
easeOutQuint : Easing
easeOutQuint =
    invert easeInQuint

{-|-}
easeInOutQuint : Easing
easeInOutQuint =
    inOut easeInQuint easeOutQuint

{-|-}
easeInSine : Easing
easeInSine =
    invert easeOutSine

{-|-}
easeOutSine : Easing
easeOutSine time =
    sin (time * (pi / 2))

{-|-}
easeInOutSine : Easing
easeInOutSine =
    inOut easeInSine easeOutSine

{-|-}
easeInExpo : Easing
easeInExpo time =
    if time == 0.0 then
        0.0
    else
        2 ^ (10 * (time - 1))

{-|-}
easeOutExpo : Easing
easeOutExpo =
    invert easeInExpo

{-|-}
easeInOutExpo : Easing
easeInOutExpo =
    inOut easeInExpo easeOutExpo

{-|-}
easeInCirc : Easing
easeInCirc =
    invert easeOutCirc

{-|-}
easeOutCirc : Easing
easeOutCirc time =
    sqrt (1 - (time - 1) ^ 2)

{-|-}
easeInOutCirc : Easing
easeInOutCirc =
    inOut easeInCirc easeOutCirc

{-|-}
easeInBack : Easing
easeInBack time =
    time * time * (2.70158 * time - 1.70158)

{-|-}
easeOutBack : Easing
easeOutBack =
    invert easeInBack

{-|-}
easeInOutBack : Easing
easeInOutBack =
    inOut easeInBack easeOutBack

{-|-}
easeInBounce : Easing
easeInBounce =
    invert easeOutBounce

{-|-}
easeOutBounce : Easing
easeOutBounce time =
    let
        a  = 7.5625
        t2 = time - (1.5 / 2.75)
        t3 = time - (2.25 / 2.75)
        t4 = time - (2.625 / 2.75)
    in
        if time < 1 / 2.75 then
            a * time * time
        else if time < 2 / 2.75 then
            a * t2 * t2 + 0.75
        else if time < 2.5 / 2.75 then
            a * t3 * t3 + 0.9375
        else
            a * t4 * t4 + 0.984375

{-|-}
easeInOutBounce : Easing
easeInOutBounce = inOut easeInBounce easeOutBounce

{-|-}
easeInElastic : Easing
easeInElastic time =
    if time == 0.0 then
        0.0
    else
        let
            s  = 0.075
            p  = 0.3
            t' = time - 1
        in
            -((2 ^ (10 * t')) * sin ((t' - s) * (2 * pi) / p))

{-|-}
easeOutElastic : Easing
easeOutElastic =
    invert easeInElastic

{-|-}
easeInOutElastic : Easing
easeInOutElastic =
    inOut easeInElastic easeOutElastic

{-| Makes an Easing function using two `Easing` functions. The first half the first `Easing` function is used, the other half the second.
-}
inOut : Easing -> Easing -> Easing
inOut e1 e2 time =
    if time < 0.5 then
        e1 (time * 2) / 2
    else
        0.5 + e2 ((time - 0.5) * 2) / 2

{-| Inverts an `Easing` function. A transition that starts fast and continues slow now starts slow and continues fast.
-}
invert : Easing -> Easing
invert easing time =
    1 - easing (1 - time)

{-| Flips an `Easing` function. A transition that looks like /, now looks like \\.
-}
flip : Easing -> Easing
flip easing time =
    easing (1 - time)

{-| Makes an `Easing` function go to the end first and then back to the start. A transition that looks like /, now looks like /\\.
-}
retour : Easing -> Easing
retour easing time =
    if time < 0.5 then
        easing (time * 2)
    else
        (time - 0.5) * 2
          |> flip easing

{-| Repeats an `Animation` infinitely

    rotate : Time -> Float
    rotate = cycle (ease linear float 0 360) second
-}
cycle : Animation a -> Animation a
cycle animation d t =
    animation 1 (t / d - toFloat (floor (t / d)))
