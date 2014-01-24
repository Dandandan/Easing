{- Daniël Heres 2014 -}

module Easing where

{-| Library for working with easing functions.

Easing functions interpolate a value over time.
They are mainly used to create animations (for user interfaces and games).

You can find graphical examples of easing functions on http://easings.net/

# Options
@docs EaseOptions, EasingOptions, EasingState

# Easing
@docs ease, easingState, easeWithFps

# Easing functions
@docs Easing,
      linear, 
      easeInQuad, easeOutQuad, easeInOutQuad,
      easeInCubic, easeOutCubic, easeInOutCubic,
      easeInQuart, easeOutQuart, easeInOutQuart,
      easeInSine, easeOutSine, easeInOutSine,
      easeInExpo, easeOutExpo, easeInOutExpo,
      easeInCirc, easeOutCirc, easeInOutCirc,
      easeInBack, easeOutBack, easeInOutBack,
      easeInPolonomial

-}

import Time (fps, timestamp, Time)

{-| Type alias for Easing functions. 
Parameters are the options of the easing, the change in value and the current time.
-}
type Easing = EasingOptions -> Time -> Time -> Float

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
linear = easeInPolonomial 1.0

easeInQuad : Easing
easeInQuad = easeInPolonomial 2.0

easeOutQuad : Easing
easeOutQuad o c t = 
    let 
        t' = t / o.duration 
    in 
        -c * t' * (t' - 2) + o.from
        
easeInOutQuad : Easing 
easeInOutQuad o c t = 
    let 
        t' = t / (o.duration / 2)
        t2 = t' - 1
    in
        if isFirstHalf o t
            then c / 2 * t' * t' + o.from
        else
            (-c / 2) * (t2 * (t2 - 2) - 1) + o.from

easeInCubic : Easing
easeInCubic = easeInPolonomial 3.0
        
easeOutCubic : Easing
easeOutCubic o c t =
    let 
        t' = t / o.duration  - 1
    in  
        c * (t' * t' * t' + 1) + o.from
        
easeInOutCubic : Easing
easeInOutCubic o c t =
    let
        t' = t / (o.duration / 2)
        t2 = t' - 2
    in
        if isFirstHalf o t then
            c / 2 * t' * t' * t' + o.from
        else
            c / 2 * (t2 * t2 * t2 + 2) + o.from
        
easeInQuart : Easing
easeInQuart = easeInPolonomial 4.0
        
easeOutQuart : Easing
easeOutQuart o c t = 
    let
        t'= t / o.duration - 1
    in
        -c * (t' * t' * t' * t' - 1) + o.from

easeInOutQuart : Easing
easeInOutQuart o c t = 
    let
        t' = t / (o.duration / 2)
        t2 = t' - 2
    in
        if isFirstHalf o t then
            c / 2 * t' * t' * t' * t' + o.from
        else
            -c / 2 * (t2 * t2 * t2 * t2 - 2) + o.from
        
easeInSine : Easing
easeInSine o c t = -c * cos(t / o.duration * (pi/2)) + c + o.from

easeOutSine : Easing
easeOutSine o c t = c * sin(t / o.duration * (pi/2)) + o.from

easeInOutSine : Easing
easeInOutSine o c t = -c / 2 * (cos (pi * t / o.duration) - 1) + o.from

easeInExpo : Easing
easeInExpo o c t = c * 2 ^ (10 * (t / o.duration - 1)) + o.from

easeOutExpo : Easing
easeOutExpo o c t = c * ( -( 2 ^ (-10 * t / o.duration )) + 1 ) + o.from

easeInOutExpo : Easing
easeInOutExpo o c t =
    let
        t' =  t / (o.duration / 2)
        t2 =  t' - 1
    in
        if isFirstHalf o t then
            c / 2 * (2 ^ (10 * (t' - 1))) + o.from
        else
            c / 2 * (-(2 ^ (-10 * t2)) + 2) + o.from
                        
easeInCirc : Easing
easeInCirc o c t =
    let
        t' = t / o.duration
    in
        -c * (sqrt(1 - t' * t') - 1) + o.from

easeOutCirc : Easing
easeOutCirc o c t =
    let
        t' = t / o.duration - 1
    in
        c * sqrt(1 - t' * t') + o.from

easeInOutCirc : Easing
easeInOutCirc o c t =
    let
        t' = t / (o.duration / 2)
        t2 = t' - 2
    in
        if isFirstHalf o t then
            -c / 2 * (sqrt(1 - t' * t') - 1) + o.from
        else
            c / 2 * (sqrt(1 - t2 * t2) + 1) + o.from

easeInBack : Easing
easeInBack o c t =
    let 
        t' = t / o.duration
        s  = 1.70158
    in
        c * t' * t' * ((s + 1) * t' - s) + o.from

easeOutBack : Easing
easeOutBack o c t =
    let 
        t' = t / o.duration - 1
        s  = 1.70158
    in
        c * (t' * t' * ((s + 1) * t' + s) +1) + o.from       

easeInOutBack : Easing
easeInOutBack o c t =
    let 
        t' = t / (o.duration / 2)
        s  = 1.70158 * 1.525
        t2 = t' - 2
    in
        if isFirstHalf o t then
            c / 2 * (t' * t' * ((s + 1) * t' - s)) + o.from
        else
            c / 2 * (t2 * t2 * ((s + 1) * t2 + s) + 2) + o.from

{-| Ease in with a polonomial function -}
easeInPolonomial : Float -> Easing
easeInPolonomial i o c t = c * (t / o.duration) ^ i + o.from

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
        {playing = p, value = if p then e o (o.to - o.from) (t - b) else o.to}

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
main = lift (asText . .value) <| ease { from = 0.0, to = 400.0, duration = 3000, easing = easeOutQuart}
