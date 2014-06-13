{- Daniël Heres 2014 -}

module Easing where

{-| Library for working creating transitions with easing functions. Easing functions interpolate a value over time. This can be a value of any type, including numbers, points and colors.

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings").

    sampleAnimation : Time -> Float
    sampleAnimation = ease easeInCubic float 0 10 second

    {- Transition from blue to red -}
    customAnimation : Time -> Color
    customAnimation = ease (\x -> x ^ 2.4) color blue red second

    {- Animate between 0 and 5 with the easeInOutQuad Easing -}
    animation1 : Time -> Float
    animation1 = ease easeInOutQuad number 0 5 second

    {- Create your own Interpolation functions -}
    vec : Interpolation Vec3
    vec from to v = from `add` ((to `sub` from) `scale` v)

    {- Use your Easing and Interpolation functions -}
    3dmovement : Time -> Vec3
    3dmovement = ease easeInQuad vec (vec3 0 0 0) (vec3 10 10 10) (3 * second)

    {- Manipulate your easing functions and animations -}
    elasticMovement : Time -> Vec3
    elasticMovement = 
        let animation = ease (retour easeOutElastic) vec (vec3 0 0 0) (vec3 10 10 10)
        in  cycle animation (3 * second)

# Easing
@docs ease

# Interpolation and Animation
@docs Interpolation, Animation

# Interpolation functions
@docs number, point2d, point3d, color

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

import Time (Time)
import Color (Color,toRgb, rgba)

{-| Type alias for Easing functions.
-}
type Easing = Float -> Float

{-| Easing `Interpolation`.
A interpolation of two values of type `a` using a Float value.

    float : Interpolation Float
    float from to v = from + (from - to) * v
-}
type Interpolation a = a -> a -> Float -> a

{-| An `Animation` is a function returns a value given a duration and the current time. 
-}
type Animation a = Time -> Time -> a

{-| Ease a value.
      Parameters are: an easing function, an interpolation function, a `from` value, a `to` value, the duration of the transition and the current (normalized) time.

      ease linear number 0 20 second     0      == 0
      ease linear number 0 20 second     second == 20
      ease linear color  blue red second second == red
      ease easeInOutQuad point2d {x=0,y=0} {x=1,y=1} second second == {x=1,y=1}
-}
ease : Easing -> Interpolation a -> a -> a -> Animation a
ease easing interpolate from to duration time = 
    interpolate from to (easing (min (time/duration) 1))

{-| Interpolation of two numbers -}
number : Interpolation number
number from to v = from + (to - from) * v

{-| Interpolation of two points in 2D -}
point2d : Interpolation { x:number, y:number }
point2d from to v = { x = number from.x to.x v , y = number from.y to.y v }

{-| Interpolation of two points in 3D -}
point3d : Interpolation { x:number, y:number, z:number }
point3d from to v = { x = number from.x to.x v , y = number from.y to.y v, z =  number from.z to.z v}

{-| Interpolation of two colors -}
color : Interpolation Color
color from to v = 
    let 
        (rgb1, rgb2)     = (toRgb from, toRgb to)
        (r1, g1, b1, a1) = (rgb1.red, rgb1.green, rgb1.blue, rgb1.alpha)
        (r2, g2, b2, a2) = (rgb2.red, rgb2.green, rgb2.blue, rgb2.alpha)
        float' from to v = round (number (toFloat from) (toFloat to) v)
    in rgba (float' r1 r2 v) (float' g1 g2 v) (float' b1 b2 v) (number a1 a2 v)

linear : Easing
linear = id

{-| A cubic bezier function using 4 parameters: x and y position of first control point, and x and y position of second control point
    See [here](http://greweb.me/glsl-transition/example/ "glsl-transitions") for examples or [here](http://cubic-bezier.com/ "tester") to test.
 -}
bezier : Float -> Float -> Float -> Float -> Easing
bezier x1 y1 x2 y2 time =
    let casteljau ps = case ps of
            [(x,y)]     -> y
            xs      -> casteljau <| zipWith (\(x1',y1') (x2',y2') -> (number x1' x2' time, number y1' y2' time)) xs (tail xs)
    in casteljau [(0,0), (x1,y1), (x2,y2), (1,1)]

easeInQuad : Easing
easeInQuad time = time ^ 2

easeOutQuad : Easing
easeOutQuad = invert easeInQuad
        
easeInOutQuad : Easing 
easeInOutQuad = inOut easeInQuad easeOutQuad

easeInCubic : Easing
easeInCubic time = time ^ 3
        
easeOutCubic : Easing
easeOutCubic = invert easeInCubic
        
easeInOutCubic : Easing
easeInOutCubic = inOut easeInCubic easeOutCubic
        
easeInQuart : Easing
easeInQuart time = time ^ 4
        
easeOutQuart : Easing
easeOutQuart = invert easeInQuart

easeInOutQuart : Easing
easeInOutQuart = inOut easeInQuart easeOutQuart

easeInQuint : Easing
easeInQuint time = time ^ 5

easeOutQuint : Easing
easeOutQuint = invert easeInQuint

easeInOutQuint : Easing
easeInOutQuint = inOut easeInQuint easeOutQuint
        
easeInSine : Easing
easeInSine = invert easeOutSine

easeOutSine : Easing
easeOutSine time = sin (time * (pi / 2))

easeInOutSine : Easing
easeInOutSine = inOut easeInSine easeOutSine

easeInExpo : Easing
easeInExpo time = 2 ^ (10 * (time - 1))

easeOutExpo : Easing
easeOutExpo = invert easeInExpo

easeInOutExpo : Easing
easeInOutExpo = inOut easeInExpo easeOutExpo
                        
easeInCirc : Easing
easeInCirc = invert easeOutCirc

easeOutCirc : Easing
easeOutCirc time =  sqrt (1 - (time - 1) ^ 2)

easeInOutCirc : Easing
easeInOutCirc = inOut easeInCirc easeOutCirc

easeInBack : Easing
easeInBack time = time * time * (2.70158 * time - 1.70158)

easeOutBack : Easing
easeOutBack = invert easeInBack 

easeInOutBack : Easing
easeInOutBack = inOut easeInBack easeOutBack
            
easeInBounce : Easing
easeInBounce = invert easeOutBounce

easeOutBounce : Easing
easeOutBounce time =
    let
        a  = 7.5625
        t2 = time - (1.5 / 2.75) 
        t3 = time - (2.25 / 2.75) 
        t4 = time - (2.65 / 2.75)
    in
        if | time < 1 / 2.75     -> a * time * time
           | time < 2 / 2.75     -> a * t2 * t2 + 0.75
           | time < 2.5 / 2.75   -> a * t3 * t3 + 0.9375
           | otherwise           -> a * t4 * t4 + 0.984375

easeInOutBounce : Easing
easeInOutBounce = inOut easeInBounce easeOutBounce

easeInElastic : Easing
easeInElastic time = 
    let
        s  = 0.075
        p  = 0.3
        t' = time - 1
    in
        -((2 ^ (10 * t')) * sin ((t' - s) * (2 * pi) / p))

easeOutElastic : Easing
easeOutElastic = invert easeInElastic

easeInOutElastic : Easing
easeInOutElastic = inOut easeInElastic easeOutElastic

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
    1 - (easing (1 - time))

{-| Flips an `Easing` function. A transition that looks like /, now looks like \\.
-}
flip : Easing -> Easing
flip easing time = easing (1 - time)

{-| Makes an `Easing` function go to the end first and then back to the start. A transition that looks like /, now looks like /\\.
-}
retour : Easing -> Easing
retour easing time =
    if time < 0.5
        then easing (time * 2)
        else (flip easing) ((time - 0.5) * 2)

{-| Repeats an `Animation` infinitely 

    rotate : Time -> Float
    rotate = cycle (ease linear number 0 360) second
-}
cycle : Animation a -> Animation a
cycle animation d t =
    animation 1 (t / d - toFloat (floor (t/d)))
