{- Daniël Heres 2014 -}

module Easing where

{-| Library for working with easing functions.

Easing functions interpolate a value over time.
They are mainly used to create animations (for user interfaces and games).

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings")

# Options
@docs EaseOptions, EasingOptions, EasingState

# Easing
@docs ease, easingState, easeWithFps, isPlaying

# Easing functions
@docs Easing,
      linear, 
      easeInQuad, easeOutQuad, easeInOutQuad,
      easeInCubic, easeOutCubic, easeInOutCubic,
      easeInQuart, easeOutQuart, easeInOutQuart,
      easeInQuint, easeInQuint, easeInQuint,
      easeInSine, easeOutSine, easeInOutSine,
      easeInExpo, easeOutExpo, easeInOutExpo,
      easeInCirc, easeOutCirc, easeInOutCirc,
      easeInBack, easeOutBack, easeInOutBack,
      easeInPolonomial, easeOutPolonomial, easeInOutPolonomial

-}

import Time (fps, timestamp, Time)

{-| Type alias for Easing functions. 
Parameters are the options of the easing, the change in value and the current time.
-}
type Easing = EasingOptions -> Time -> Float

{-| Options for easing.
* <b>from</b> is value at the start
* <b>to</b> is the value at the end
* <b>duration</b> is the time the easing takes
* <b>easing</b> is the easing function
-}
type EaseOptions = 
    { from     : Float
    , to       : Float
    , duration : Time
    , easing   : Easing
    }

{-| Options for easing.
Excludes an easing function, so you can use it to ease
without the `ease` function
-}

type EasingOptions = 
    { from     : Float
    , to       : Float
    , duration : Time
    }

{-| Represents the state of the easing
* <b>value</b> is the value at the current time
* <b>playing</b> whether the easing function is in progress or not
-}
type EasingState =
    { value   : Float
    , playing : Bool
    }

{-| Linear easing, doesn't accelerate -}
linear : Easing
linear = easeInPolonomial 1

easeInQuad : Easing
easeInQuad = easeInPolonomial 2

easeOutQuad : Easing
easeOutQuad = easeOutPolonomial 2
        
easeInOutQuad : Easing 
easeInOutQuad = easeInOutPolonomial 2

easeInCubic : Easing
easeInCubic = easeInPolonomial 3
        
easeOutCubic : Easing
easeOutCubic = easeOutPolonomial 3
        
easeInOutCubic : Easing
easeInOutCubic = easeInOutPolonomial 3
        
easeInQuart : Easing
easeInQuart = easeInPolonomial 4
        
easeOutQuart : Easing
easeOutQuart = easeOutPolonomial 4

easeInOutQuart : Easing
easeInOutQuart = easeOutPolonomial 4

easeInQuint : Easing
easeInQuint = easeInPolonomial 5

easeOutQuint : Easing
easeOutQuint = easeOutPolonomial 5

easeInOutQuint : Easing
easeInOutQuint = easeInOutPolonomial 5
        
easeInSine : Easing
easeInSine o t = let c = o.to - o.from in -c * cos(t / o.duration * (pi/2)) + c + o.from

easeOutSine : Easing
easeOutSine o t = let c = o.to - o.from in  c * sin(t / o.duration * (pi/2)) + o.from

easeInOutSine : Easing
easeInOutSine o t = let c = o.to - o.from in -c / 2 * (cos (pi * t / o.duration) - 1) + o.from

easeInExpo : Easing
easeInExpo o t = let c = o.to - o.from in c * 2 ^ (10 * (t / o.duration - 1)) + o.from

easeOutExpo : Easing
easeOutExpo o t = let c = o.to - o.from in c * ( -( 2 ^ (-10 * t / o.duration )) + 1 ) + o.from

easeInOutExpo : Easing
easeInOutExpo o t =
    let
        t' =  t / (o.duration / 2)
        t2 =  t' - 1
        c = o.to - o.from
    in
        if isFirstHalf o t then
            c / 2 * (2 ^ (10 * (t' - 1))) + o.from
        else
            c / 2 * (-(2 ^ (-10 * t2)) + 2) + o.from
                        
easeInCirc : Easing
easeInCirc o t =
    let
        t' = t / o.duration
        c = o.to - o.from
    in
        -c * (sqrt(1 - t' * t') - 1) + o.from

easeOutCirc : Easing
easeOutCirc o t =
    let
        t' = t / o.duration - 1
        c = o.to - o.from
    in
        c * sqrt(1 - t' * t') + o.from

easeInOutCirc : Easing
easeInOutCirc o t =
    let
        t' = t / (o.duration / 2)
        t2 = t' - 2
        c = o.to - o.from
    in
        if isFirstHalf o t then
            -c / 2 * (sqrt(1 - t' * t') - 1) + o.from
        else
            c / 2 * (sqrt(1 - t2 * t2) + 1) + o.from

easeInBack : Easing
easeInBack o t =
    let 
        t' = t / o.duration
        s  = 1.70158
        c = o.to - o.from
    in
        c * t' * t' * ((s + 1) * t' - s) + o.from

easeOutBack : Easing
easeOutBack o t =
    let 
        t' = t / o.duration - 1
        s  = 1.70158
        c = o.to - o.from
    in
        c * (t' * t' * ((s + 1) * t' + s) +1) + o.from       

easeInOutBack : Easing
easeInOutBack o t =
    let 
        t' = t / (o.duration / 2)
        s  = 1.70158 * 1.525
        t2 = t' - 2
        c = o.to - o.from
    in
        if isFirstHalf o t then
            c / 2 * (t' * t' * ((s + 1) * t' - s)) + o.from
        else
            c / 2 * (t2 * t2 * ((s + 1) * t2 + s) + 2) + o.from

{-| Ease in with a polonomial function -}
easeInPolonomial : Int -> Easing
easeInPolonomial i o t = let c = o.to - o.from in c * (t / o.duration) ^ (toFloat i) + o.from

{-| Ease out with a polonomial function -}
easeOutPolonomial : Int -> Easing
easeOutPolonomial i o t = 
    let
        t' = t / o.duration - 1
        c = o.to - o.from
    in
        if i `mod` 2 == 0 then
            -c * (t' * t' * t' * t' - 1) + o.from
        else
            c * (t' ^  (toFloat i) + 1) + o.from

{-| Ease in and out with a polonomial function -}            
easeInOutPolonomial : Int -> Easing
easeInOutPolonomial i o t = 
    let
        t' = t * 2
        t2 = t' / 2
    in
        if isFirstHalf o t then
            easeInPolonomial i o t'
        else
            easeOutPolonomial i o t2

{-| Get the state and value of the easing at the current time
    Assumes the time is normalized, it started at 0.
    Parameters are the options of the easing, the current time, the begin time
    and the easing function.
-}
easingState : EasingOptions -> Time -> Time -> Easing -> EasingState
easingState o t b e = 
    let 
        p = isPlaying o (t - b)
    in
        {playing = p, value = if p then e o (t - b) else o.to}

{-| The easing is still playing at the current time -}
isPlaying : EasingOptions -> Float -> Bool
isPlaying o t = t < o.duration

isFirstHalf : EasingOptions -> Float -> Bool
isFirstHalf o t = t < o.duration / 2

{-| Apply an ease function with 60 frames per second.
Returns a signal with an `EasingState`.

```haskell
  ease { from = 0.0, to = 400.0, duration = 3000, easing = linear}
```

-} 
ease : EaseOptions -> Signal EasingState
ease = easeWithFps 60

{-| Apply an ease function. Parameters are frames per second and the options of the easing -}
easeWithFps : Int -> EaseOptions -> Signal EasingState
easeWithFps f o =
    let 
        b = lift fst <| timestamp (constant ())
        e ((t, _),b) _ = easingState {o - easing} t b o.easing
    in
        foldp e {value = o.from, playing = True} (lift2 (,) (timestamp (fps f)) b)
