import Easing (..)
import Easing as E
import Graphics.Collage (..)
import Mouse
import Window
import Maybe (maybe)

duration = 2000

animation = { from = 0, to = 1, duration = duration}

mod' : Float -> Float -> Float
mod' n d = let f = floor (n / d) in n - (toFloat f) * d

sun : Path
sun = circle 30

remember = foldp (::) []

colors = [blue, orange, yellow, green, purple]

list e f xs = case xs of
    [] -> e
    _  -> f xs

head' xs = 
    case xs of
        [] -> Nothing
        x::xs  -> Just x

index i xs = list Nothing head' (drop i xs)

update (i,_) xs (dx,dy) = map (\((t, (x,y)),clicks) -> 
    let 
        currentTime = i - t
        click' = clicks % length colors
        color = maybe orange identity <| index click' colors
        playing = isPlaying animation currentTime
        a = if playing then ease (inAndOut E.linear) animation currentTime else 0
        sun' = filled color sun
    in
        move (toFloat x - toFloat dx / 2, toFloat dy /2 - toFloat y) <| alpha a sun') xs

pulse = update <~ timestamp (fps 25)
                ~ remember (Mouse.clicks `sampleOn` ((,) <~ timestamp Mouse.position ~ count Mouse.clicks))
                ~ Window.dimensions

main = (\fs (x,y) -> collage x y fs)
             <~ pulse
              ~ Window.dimensions
