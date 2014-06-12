import Easing (..)
import Graphics.Collage (..)
import Time(..)
import Graphics.Input (..)

easing : Input Easing
easing = input linear

click : Input ()
click = input ()

play : Easing -> Time -> Time -> Element
play e b t =
    let
        x = ease e number { from = -180, to = 180, duration = second } (t - b)
        c = ease e color { from = blue, to = red, duration = second } (t - b)
        movingCircle = circle 10 |> filled c |> moveX x
     in
        collage 450 100 [movingCircle]

display : Easing -> Time -> Time -> Element
display e b t =
    flow down
        [ [markdown|<h1>Easing Functions</h1>|]
        , play e b t
        , easingChoice
        , button click.handle () "Play"
        ]

main : Signal Element
main = display <~ easing.signal ~ (fst <~ timestamp click.signal) ~ every 16 

easingChoice : Element
easingChoice =
  dropDown easing.handle
    [ ("linear", linear)
    , ("easeInQuad", easeInQuad)
    , ("easeOutQuad", easeOutQuad)
    , ("easeInOutQuad", easeInOutQuad)
    , ("easeInCubic", easeInCubic)
    , ("easeOutCubic", easeOutCubic)
    , ("easeInOutCubic", easeInOutCubic)
    , ("easeInQuart", easeInQuart)
    , ("easeOutQuart", easeOutQuart)
    , ("easeInOutQuart", easeInOutQuart)
    , ("easeInQuint", easeInQuint)
    , ("easeOutQuint", easeOutQuint)
    , ("easeInOutQuint", easeInOutQuint)
    , ("easeInSine", easeInSine)
    , ("easeOutSine", easeOutSine)
    , ("easeInOutSine", easeInOutSine)
    , ("easeInExpo", easeInExpo)
    , ("easeOutExpo", easeOutExpo)
    , ("easeInOutExpo", easeInOutExpo)
    , ("easeInCirc", easeInCirc)
    , ("easeOutCirc", easeOutCirc)
    , ("easeInOutCirc", easeInOutCirc)
    , ("easeInBack", easeInBack)
    , ("easeOutBack", easeOutBack)
    , ("easeInOutBack", easeInOutBack)
    , ("easeInBounce", easeInBounce)
    , ("easeOutBounce", easeOutBounce)
    , ("easeInOutBounce", easeInOutBounce)
    , ("easeInElastic", easeInElastic)
    , ("easeOutElastic", easeOutElastic)
    , ("easeInOutElastic", easeInOutElastic)
    ]
