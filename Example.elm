import open Easing
import open Graphics.Collage

--from tween library

{-| Move the x and y positions of a form with `EaseOptions` -}
tweenWith : EaseOptions -> EaseOptions -> Form -> Signal Form
tweenWith ox oy f = lift2 (\x y -> move (x.value, y.value) f) (ease ox) (ease oy)

{-| Tween form to (x,y) with a duration and an easing function -}
tween : (Float, Float) -> Time -> Easing -> Form -> Signal Form
tween (x, y) d e f =
    let
        x' = {from = f.x, to = x, duration = d, easing = e}
        y' = {from = f.y, to = y, duration = d, easing = e}
    in
        tweenWith x' y' f


main = lift (collage 500 500) <| combine 
            [ tween (100, 100) 2000 easeInOutElastic <| filled blue (circle 100)
            , tween (0, 200) 2000 easeInOutElastic <| filled red (rect 40 70)
            , tween (200, 200) 1000 easeSaw <| toForm <| plainText "aaa"
            , tween (0, 200) 5000 (keyFrames linear [(0, 0.0), (500,0.3), (1500,0.6),(3000,0.8)]) <| filled orange (circle 10)
            ]
