{- Daniël Heres 2014 -}

module Easing where

{-| Library for working creating transitions with easing functions. Easing functions interpolate a value over time.

You can find graphical examples of easing functions on [easings.net](http://easings.net/ "Easings").


    linear : Easing
    linear x = x

    sampleAnimation : Time -> Float
    sampleAnimation = ease easeInCubic { from = 0, to = 10, duration = second }

    customAnimation : Time -> Float
    customAnimation = ease (\x -> x ^ 2.4) { from = 0, to = 200, duration = 4 * second }

    tenSteps : Easing
    tenSteps = toFloat (floor (x * 10)) / 10

    tenStepsAnimation : Time -> Float
    tenStepsAnimation = ease tenSteps { from = 0, to = 5, duration = second }


# Transition settings
@docs Transition

# Easing
@docs ease

#Easing function manipulation
@docs invert, inAndOut, inOut, flip

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
      easeInElastic, easeOutElastic, easeInOutElastic
-}

import Time (fps, timestamp, Time)

{-| Type alias for Easing functions. 
Parameters is the current time fraction.

      linear : Easing
      linear time = time
-}
type Easing = Float -> Float

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
    transition.from + (transition.to - transition.from) * easing (min (time/transition.duration) 1)

{-| Linear easing function, doesn't accelerate -}
linear : Easing
linear = id

easeInQuad : Easing
easeInQuad time = time ^ 2

easeOutQuad : Easing
easeOutQuad = invert easeInQuad
        
easeInOutQuad : Easing 
easeInOutQuad = inOut easeInQuad easeOutQuad

easeInCubic : Easing
easeInCubic time = time ^ 3
        
easeOutCubic : Easing
easeOutCubic = invert easeOutCubic
        
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

{-| Makes an Easing function using two easing functions. The first half the first `Easing` function is used, the other half the second.
-}
inOut : Easing -> Easing -> Easing
inOut e1 e2 time =
    if time < 0.5 then
        e1 (time * 2) / 2
    else
        0.5 + e2 ((time - 0.5) * 2) / 2

{-| Inverts an easing function. A transition that starts fast and continues slow now starts slow and continues fast.
-}
invert : Easing -> Easing
invert easing time =
    1 - (easing (1 - time))

{-| Flips an easing function. A transition that looks like /, now looks like \\.
-}
flip : Easing -> Easing
flip easing time = easing (1 - time)

{-| Makes an Easing function go in and out. A transition that looks like /, now looks like /\\.
-}
inAndOut : Easing -> Easing
inAndOut easing time =
    if time < 0.5
        then easing (time * 2)
        else (flip easing) ((time - 0.5) * 2)
