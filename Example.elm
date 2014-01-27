import open Easing
import open Graphics.Collage
import Mouse
import Window

d = { w = 500
    , h = 500
    }

duration = 2000
easing : Time -> EasingState
easing = easingState <| { from = 0, to = 1, duration = duration, easing = linear}

mod' : Float -> Float -> Float
mod' n d = let f = floor (n / d) in n - (toFloat f) * d

sun : Form
sun = filled yellow <| circle 30

pulse = 
    let 
        update i xs (dx,dy) = map (\(x,y) -> move (toFloat x - toFloat dx /2, toFloat dy /2 - toFloat y) <| alpha (easing (mod' i duration)).value sun) xs
    in
        update <~ foldp (+) 0 (fps 60)
                ~ foldp (::) [] (Mouse.clicks `sampleOn` Mouse.position)
                ~ Window.dimensions

main : Signal Element
main = (\fs (x,y) -> collage x y fs)
             <~ pulse
              ~ Window.dimensions
