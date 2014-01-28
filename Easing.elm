{- Daniël Heres 2014 -}

module Easing where

{-| Library for working with easing functions.

Easing functions interpolate a value over time.
They are mainly used to create animations (for user interfaces and games).

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings")

# Options
@docs EaseOptions, EasingOptions

# Easing
@docs isPlaying

# Easing functions
@docs Easing,
      linear,
      keyFrames,
      easeFrom, easeTo,
      easeInQuad, easeOutQuad, easeInOutQuad,
      easeInCubic, easeOutCubic, easeInOutCubic,
      easeInQuart, easeOutQuart, easeInOutQuart,
      easeInQuint, easeInQuint, easeInQuint,
      easeInSine, easeOutSine, easeInOutSine,
      easeInExpo, easeOutExpo, easeInOutExpo,
      easeInCirc, easeOutCirc, easeInOutCirc,
      easeInBack, easeOutBack, easeInOutBack,
      easeInBounce, easeOutBounce, easeInOutBounce,
      easeInElastic, easeOutElastic, easeInOutElastic,
      easeInPolynomial, easeOutPolynomial, easeInOutPolynomial

#Easing function manipulation
@docs invert, inAndOut

-}

import Time (fps, timestamp, Time)

{-| Type alias for Easing functions. 
Parameters are the options of the easing and the current time.
-}
type Easing = EasingOptions {} -> Time -> Float

{-| Options for easing.

* <b>from</b> is value at the start
* <b>to</b> is the value at the end
* <b>duration</b> is the time the easing takes
* <b>easing</b> is the easing function

-}
type EaseOptions r = 
    { r |
      easing   : Easing
    }

{-| Options for easing.
Excludes an easing function, so you can use it to ease
without the `ease` function
-}

type EasingOptions r = 
    { r | 
      from     : Float
    , to       : Float
    , duration : Time
    }

{-| Linear easing function, doesn't accelerate -}
linear : Easing
linear = easeInPolynomial 1

{-| Create an easing function from keyframes.

The first argument is an easing function.

Takes as second argument list of pairs with the `Time` as first element of the pair and the fraction as second element.

```haskell
{- With linear interpolation -}
keyFrames linear [(0, 0.0), (200,0.5), (300,0.7)]

{- Without linear interpolation (stop-motion) -}
keyFrames easeFrom [(0, 0.0), (200,0.5), (300,0.7)]
```

-}
keyFrames : Easing -> [(Time, Float)]  -> Easing
keyFrames easing fs o t = 
    let
        c = o.to - o.from
    in
        case fs of
          [] -> o.to
          ((i,f)::[]) -> easing { from     = o.from + c * f
                                , to       = o.to
                                , duration = o.duration - i
                                } (t - i)
          ((i,f)::(i', f')::xs) -> 
                if | i' <= t    -> keyFrames easing ((i', f')::xs) o t
                   | otherwise  -> easing { from     = o.from + c * f
                                          , to       = o.from + c * f'
                                          , duration = i' - i
                                          } (t - i)

{-|Saw-like motion -}
easeSaw : Easing
easeSaw o = 
    let 
        k = keyFrames linear <| map (\(x,y) -> (x * o.duration, y)) 
                                [(0,0.0), (0.2, 0.4), (0.4, 0.3), (0.6, 0.7), (0.8, 0.6)] 
    in
        k o
                     

easeInQuad : Easing
easeInQuad = easeInPolynomial 2

easeOutQuad : Easing
easeOutQuad = easeOutPolynomial 2
        
easeInOutQuad : Easing 
easeInOutQuad = easeInOutPolynomial 2

easeInCubic : Easing
easeInCubic = easeInPolynomial 3
        
easeOutCubic : Easing
easeOutCubic = easeOutPolynomial 3
        
easeInOutCubic : Easing
easeInOutCubic = easeInOutPolynomial 3
        
easeInQuart : Easing
easeInQuart = easeInPolynomial 4
        
easeOutQuart : Easing
easeOutQuart = easeOutPolynomial 4

easeInOutQuart : Easing
easeInOutQuart = easeOutPolynomial 4

easeInQuint : Easing
easeInQuint = easeInPolynomial 5

easeOutQuint : Easing
easeOutQuint = easeOutPolynomial 5

easeInOutQuint : Easing
easeInOutQuint = easeInOutPolynomial 5
        
easeInSine : Easing
easeInSine o t = let c = o.to - o.from in -c * cos(t / o.duration * (pi/2)) + c + o.from

easeOutSine : Easing
easeOutSine = invert easeInSine

easeInOutSine : Easing
easeInOutSine o t = easeInOut o t easeInSine easeOutSine

easeInExpo : Easing
easeInExpo o t = let c = o.to - o.from in c * 2 ^ (10 * (t / o.duration - 1)) + o.from

easeOutExpo : Easing
easeOutExpo = invert easeInExpo

easeInOutExpo : Easing
easeInOutExpo o t = easeInOut o t easeInExpo easeOutExpo
                        
easeInCirc : Easing
easeInCirc = invert easeOutCirc

easeOutCirc : Easing
easeOutCirc o t= 
    let
        t' = t / o.duration - 1
        c = o.to - o.from
    in
        c * (sqrt(1 - t' * t')) + o.from

easeInOutCirc : Easing
easeInOutCirc o t = easeInOut o t easeInCirc easeOutCirc

easeInBack : Easing
easeInBack o t =
    let 
        t' = t / o.duration
        s  = 1.70158
        c = o.to - o.from
    in
        c * t' * t' * ((s + 1) * t' - s) + o.from

easeOutBack : Easing
easeOutBack = invert easeInBack 

easeInOutBack : Easing
easeInOutBack o t = easeInOut o t easeInBack easeOutBack
            
easeInBounce : Easing
easeInBounce = invert easeOutBounce

easeOutBounce : Easing
easeOutBounce o t =
    let
        c = o.to - o.from
        a  = 7.5625
        t1 = t / o.duration
        t2 = t1 - (1.5 / 2.75) 
        t3 = t1 - (2.25 / 2.75) 
        t4 = t1 - (2.65 / 2.75) 
    in
        if | t1 < 1 / 2.75     -> c * (a * t1 * t1) + o.from
           | t1 < 2 / 2.75     -> c * (a * t2 * t2 + 0.75) + o.from
           | t1 < 2.5 / 2.75   -> c * (a * t3 * t3 + 0.9375) + o.from
           | otherwise         -> c * (a * t4 * t4 + 0.984375) + o.from

easeInOutBounce : Easing
easeInOutBounce o t = easeInOut o t easeInBounce easeOutBounce

easeInElastic : Easing
easeInElastic o t = 
    let 
        c  = o.to - o.from
        s  = 0.075
        p  = 0.3
        t' = t / o.duration - 1
    in
        -((2 ^ (10 * t')) * sin ((t' - s) * (2 * pi) / p)) * c + o.from

easeOutElastic : Easing
easeOutElastic = invert easeInElastic

easeInOutElastic : Easing
easeInOutElastic o t = easeInOut o t easeInElastic easeOutElastic

easeInOut : EasingOptions {} -> Time -> Easing -> Easing -> Float
easeInOut o t e1 e2 = let c = o.to - o.from in
    if isFirstHalf o t then
        e1 o (t * 2) / 2 + o.from
    else
        e2 o (t * 2 - o.duration) / 2 + c / 2 + o.from

invert : Easing -> Easing
invert e o t = let c = o.to - o.from in c - e o (o.duration - t)

inAndOut : Easing -> Easing
inAndOut e o t = let c = o.to - o.from in
    if isFirstHalf o t then
        e o (t * 2) / 2 + o.from
    else
        c - e o (t * 2) /2 + o.from

{-| Doesn't ease, but stays at from untill the end -}
easeFrom : Easing
easeFrom o _ = o.from

{-| Is at the to value immediately -}
easeTo : Easing
easeTo o _ = o.to
 
{-| Ease in with a polynomial function -}
easeInPolynomial : Int -> Easing
easeInPolynomial i o t = let c = o.to - o.from in c * (t / o.duration) ^ (toFloat i) + o.from

{-| Ease out with a polynomial function -}
easeOutPolynomial : Int -> Easing
easeOutPolynomial i = invert (easeInPolynomial i)

{-| Ease in and out with a polynomial function -}            
easeInOutPolynomial : Int -> Easing
easeInOutPolynomial i o t = easeInOut o t (easeInPolynomial i) (easeOutPolynomial i)

{-| Get the value at the current time
-}
ease : EaseOptions (EasingOptions {}) -> Time -> Float
ease o t = 
    let 
        p = isPlaying o t
    in 
        if p then o.easing { o - easing } t else o.to

{-| The easing is still playing at the current time -}
isPlaying : EasingOptions (EaseOptions {}) -> Float -> Bool
isPlaying o t = t < o.duration

isFirstHalf : EasingOptions {} -> Float -> Bool
isFirstHalf o t = t < o.duration / 2
