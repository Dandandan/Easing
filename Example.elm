import Easing (..)
import Graphics.Collage (..)
import Mouse
import Window

update : (Time, Time) -> (Time, (Int, Int)) -> (Int, Int) -> Form
update (t,_) (b, (x,y)) (dx,dy) = 
    let 
        a = ease easeInOutQuart point2d {x=0, y=0} {x=200, y = -200} second (t - b)
        c = ease easeInOutQuart color blue red second (t - b)
        sun' = circle 30 |> filled c
    in
        move (a.x, a.y) <| move (toFloat x - toFloat dx / 2, toFloat dy /2 - toFloat y) <| sun'

pulse : Signal Form
pulse = update <~ timestamp (fps 30)
                ~ Mouse.clicks `sampleOn` timestamp Mouse.position
                ~ Window.dimensions

main : Signal Element
main = (\fs (x,y) -> collage x y [fs])
             <~ pulse
              ~ Window.dimensions
