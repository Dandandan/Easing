{- Daniël Heres 2014 -}

module Easing where

{-| Library for working creating animations and transitions with easing functions. Easing functions interpolate a value over time.

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings")

# Transition settings
@docs Transition

# Easing
@docs ease, isPlaying

#Easing function manipulation
@docs invert, inAndOut, inOut, flip

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
      easeInElastic, easeOutElastic, easeInOutElastic,
      easeInPolynomial, easeOutPolynomial, easeInOutPolynomial

-}

import Time (fps, timestamp, Time)

{-| Type alias for Easing functions. 
Parameters are the `Transition` and the current `Time`.

      linear : Easing
      linear transition time = transition.from + transition.to * (time / transition.duration)
-}
type Easing = Transition -> Time -> Float

{-| Easing `Transition`.

* `from`, the value value at the start
* `to`, the value at the end
* `duration`, the duration of the transition

-}
type Transition = { from : Float, to : Float, duration : Time }

{-| Get the value at the current time

      ease linear { from = 0, to = 20, duration = second } 0      == 0
      ease linear { from = 0, to = 20, duration = second } second == 20
-}
ease : Easing -> Transition -> Time -> Float
ease easing transition time = 
    easing transition (min time transition.duration)

{-| Create an easing function from keyframes.

The first argument is an easing function.

Takes as second argument list of pairs with the `Time` as first element of the pair and the fraction as second element.

      keyFrames linear [(0, 0.0), (200,0.5), (300,0.7)]

-}
keyFrames : Easing -> [(Time, Float)] -> Easing
keyFrames easing frames transition time = 
    let
        d = difference transition
    in
        case frames of
          [] -> transition.to
          ((i,f)::[]) -> easing { from     = transition.from + d * f
                                , to       = transition.to
                                , duration = transition.duration - i
                                } (time - i)
          ((i,f)::(i', f')::xs) -> 
                if | i' <= time -> keyFrames easing ((i', f')::xs) transition time
                   | otherwise  -> easing { from     = transition.from + d * f
                                          , to       = transition.from + d * f'
                                          , duration = i' - i
                                          } (time - i)

{-| The easing is still playing at the current time 

      isPlaying { from = 0, to = 10, duration = second } (0.5 * second) == True
      isPlaying { from = 0, to = 10, duration = second } (2 * second)   == False
-}
isPlaying : Transition -> Time -> Bool
isPlaying transition time =
    time < transition.duration

{-| Linear easing function, doesn't accelerate -}
linear : Easing
linear =
    easeInPolynomial 1

easeInQuad : Easing
easeInQuad =    
    easeInPolynomial 2

easeOutQuad : Easing
easeOutQuad =
    easeOutPolynomial 2
        
easeInOutQuad : Easing 
easeInOutQuad =
    easeInOutPolynomial 2

easeInCubic : Easing
easeInCubic =
    easeInPolynomial 3
        
easeOutCubic : Easing
easeOutCubic =
    easeOutPolynomial 3
        
easeInOutCubic : Easing
easeInOutCubic =
    easeInOutPolynomial 3
        
easeInQuart : Easing
easeInQuart =
    easeInPolynomial 4
        
easeOutQuart : Easing
easeOutQuart =
    easeOutPolynomial 4

easeInOutQuart : Easing
easeInOutQuart =
    easeOutPolynomial 4

easeInQuint : Easing
easeInQuint =
    easeInPolynomial 5

easeOutQuint : Easing
easeOutQuint =
    easeOutPolynomial 5

easeInOutQuint : Easing
easeInOutQuint =
    easeInOutPolynomial 5
        
easeInSine : Easing
easeInSine transition time = 
    -(difference transition) * cos (time / transition.duration * (pi/2)) + difference transition + transition.from

easeOutSine : Easing
easeOutSine =
    invert easeInSine

easeInOutSine : Easing
easeInOutSine =
    inOut easeInSine easeOutSine

easeInExpo : Easing
easeInExpo transition time =
    difference transition * 2 ^ (10 * (time / transition.duration - 1)) + transition.from

easeOutExpo : Easing
easeOutExpo =
    invert easeInExpo

easeInOutExpo : Easing
easeInOutExpo =
    inOut easeInExpo easeOutExpo
                        
easeInCirc : Easing
easeInCirc =
    invert easeOutCirc

easeOutCirc : Easing
easeOutCirc transition time = 
    let
        t' = time / transition.duration - 1
    in
        difference transition * (sqrt(1 - t' * t')) + transition.from

easeInOutCirc : Easing
easeInOutCirc =
    inOut easeInCirc easeOutCirc

easeInBack : Easing
easeInBack transition time =
    let 
        t' = time / transition.duration
        s  = 1.70158
        c = transition.to - transition.from
    in
        c * t' * t' * ((s + 1) * t' - s) + transition.from

easeOutBack : Easing
easeOutBack =
    invert easeInBack 

easeInOutBack : Easing
easeInOutBack =
    inOut easeInBack easeOutBack
            
easeInBounce : Easing
easeInBounce =
    invert easeOutBounce

easeOutBounce : Easing
easeOutBounce transition time =
    let
        c = transition.to - transition.from
        a  = 7.5625
        t1 = time / transition.duration
        t2 = t1 - (1.5 / 2.75) 
        t3 = t1 - (2.25 / 2.75) 
        t4 = t1 - (2.65 / 2.75) 
    in
        if | t1 < 1 / 2.75     -> c * (a * t1 * t1) + transition.from
           | t1 < 2 / 2.75     -> c * (a * t2 * t2 + 0.75) + transition.from
           | t1 < 2.5 / 2.75   -> c * (a * t3 * t3 + 0.9375) + transition.from
           | otherwise         -> c * (a * t4 * t4 + 0.984375) + transition.from

easeInOutBounce : Easing
easeInOutBounce =
    inOut easeInBounce easeOutBounce

easeInElastic : Easing
easeInElastic transition time = 
    let
        s  = 0.075
        p  = 0.3
        t' = time / transition.duration - 1
    in
        -((2 ^ (10 * t')) * sin ((t' - s) * (2 * pi) / p)) * difference transition + transition.from

easeOutElastic : Easing
easeOutElastic =
    invert easeInElastic

easeInOutElastic : Easing
easeInOutElastic =
    inOut easeInElastic easeOutElastic

{-| Makes an Easing function use . A transition that looks like /, now looks like /\\.
You can provide two different easing functions.  
-}
inOut : Easing -> Easing -> Easing
inOut e1 e2 transition time =
    if isFirstHalf transition time then
        e1 transition (time * 2) / 2 + transition.from
    else
        e2 transition (time * 2 - transition.duration) / 2 
            + difference transition / 2 + transition.from

{-| Inverts an easing function. A transition that starts fast and continues slow now starts slow and continues fast.
-}
invert : Easing -> Easing
invert easing transition time =
    difference transition - easing transition (transition.duration - time)

{-| Flips an easing function. A transition that looks like /, now looks like \\.
-}
flip : Easing -> Easing
flip easing transition =
    easing { transition | from <- transition.to, to <- transition.from } 

{-| Makes an Easing function go in and out. A transition that looks like /, now looks like /\\.
-}
inAndOut : Easing -> Easing
inAndOut easing transition time =
    if isFirstHalf transition time
        then easing transition (time * 2)
        else (flip easing) transition (time * 2 - transition.duration)

{-| Doesn't ease, but stays at `from` untill the end -}
easeFrom : Easing
easeFrom transition _ =
    transition.from

{-| Is at the `to` value immediately -}
easeTo : Easing
easeTo transition _ =
    transition.to
 
{-| Ease in with a polynomial function -}
easeInPolynomial : Int -> Easing
easeInPolynomial i transition time =
    difference transition * (time / transition.duration) ^ (toFloat i) + transition.from

{-| Ease out with a polynomial function -}
easeOutPolynomial : Int -> Easing
easeOutPolynomial =
    invert . easeInPolynomial

{-| Ease in and out with a polynomial function -}
easeInOutPolynomial : Int -> Easing
easeInOutPolynomial i =
    inOut (easeInPolynomial i) (easeOutPolynomial i)

isFirstHalf : Transition -> Time -> Bool
isFirstHalf transition time = 
    time < transition.duration / 2

difference : Transition -> Float
difference transition = 
    transition.to - transition.from

