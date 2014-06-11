import Easing
import Time(second)
import ElmTest.Test (test, Test, suite)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.Element (runDisplay)

tests : Test
tests = suite "Test Easing"
        [ test "Linear ease begin" (assertEqual (Easing.ease Easing.linear {from = 0, to = 2, duration = 1} 0) 0)
        , test "Linear ease end" (assertEqual (Easing.ease Easing.linear {from = 0, to = 2, duration = 1} 2) 2)
        , test "Linear ease begin" (assertEqual (Easing.ease Easing.linear {from = -2, to = 2, duration = 1} 0) -2)
        , test "Linear ease end" (assertEqual (Easing.ease Easing.linear {from = -2, to = 2, duration = 1} 2) 2)
        , test "Linear ease half" (assertEqual (Easing.ease Easing.linear {from = -2, to = 2, duration = 1} 0.5) 0)
        , test "Linear ease begin invert" (assertEqual (Easing.ease (Easing.invert Easing.linear) {from = -2, to = 2, duration = 1} 0) -2)
        , test "Linear ease half invert" (assertEqual (Easing.ease (Easing.invert Easing.linear) {from = -2, to = 2, duration = 1} 0.5) 0)
        , test "Linear ease end invert" (assertEqual (Easing.ease (Easing.invert Easing.linear) {from = -2, to = 2, duration = 1} 1) 2)
        , test "Linear ease inAndOut" (assertEqual (Easing.ease (Easing.invert Easing.linear) {from = -2, to = 2, duration = 1} 1) 2)
        ]

main : Element
main = runDisplay tests

