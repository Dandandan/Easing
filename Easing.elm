{- Daniël Heres 2014 -}

module Easing where

{-| Library for working with easing.

# Options
@docs EaseOptions, EasingState

# Easing
@docs ease

# Easing functions
@docs linear, easeInQuad, easeOutQuad, easeInOutQuad, easeInCubic, easeInOutCubic

-}

import Time (fps, timestamp)

type Easing = EasingOptions -> Float -> Float -> Float

{-| Options for easing.
* <b>from</b> is value at the start
* <b>to</b> is the value at the end
* <b>duration</b> is the time the easing takes
* <b>easing</b> is the easing function
-}
type EaseOptions = 
    { from     : Float
    , to       : Float
    , duration : Float
    , easing   : Easing
    }

type EasingOptions = 
    { from     : Float
    , to       : Float
    , duration : Float
    }

type PlayState =
    { playing : Bool
    }

{-| Represents the state of the easing
* <b>value</b> is the value at the current time
* <b>playing</b> whether the easing function is in progress or not
-}
type EasingState =
    { value   : Float
    , playing : Bool
    }
    
easeStart : EasingState
easeStart = 
    { value    = 0.0
    , playing = True
    }

linear : Easing
linear o c t = c * t / o.duration + o.from

easeInQuad : Easing
easeInQuad o c t = c * (t / o.duration) ^ 2 + o.from

easeOutQuad : Easing
easeOutQuad o c t = 
    let 
        t' = t / o.duration 
    in 
        -c * t'*(t'-2) + o.from
        
easeInOutQuad : Easing 
easeInOutQuad o c t = 
    let 
        t' = t / (o.duration / 2)
        t2 = t' - 1
    in
        if isFirstHalf o t then c / 2 * t' * t' + o.from else (-c / 2) * (t2 * (t2 - 2) - 1) + o.from

easeInCubic : Easing
easeInCubic o c t =
    let 
        t' = t / o.duration 
    in  
        c * t' * t' * t' + o.from
        
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
        if isFirstHalf o t then c / 2 * t' * t' * t' + o.from else c / 2 * (t2 * t2 * t2 + 2) + o.from
        
        
isPlaying : EasingOptions -> Float -> Bool
isPlaying o t = t < o.duration

isFirstHalf : EasingOptions -> Float -> Bool
isFirstHalf o t = t < o.duration / 2

{-| Apply an ease function
    `tween { from = 0.0, to = 400.0, duration = 3000, easing = linear}`
Returns a signal with an `EasingState`.
-} 
ease : EaseOptions -> Signal EasingState
ease o = 
    let 
        b = lift fst <| timestamp (constant ())
        s x = lift2 (,) x b
        e ((t, _),b) _ = 
            let n = o.easing {o - easing} (o.to - o.from) (t-b)
                p = isPlaying {o - easing} (t-b)
            in {playing = p, value = if p then n else o.to}
    in  
        foldp e easeStart (s (timestamp (fps 60)))
