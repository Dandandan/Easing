Easing (animation in Elm)
======

`Easing` is a library for working creating simple animations with easing functions. Easing functions interpolate a value over time. This can be a value of any type, including numbers, points and colors.


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
