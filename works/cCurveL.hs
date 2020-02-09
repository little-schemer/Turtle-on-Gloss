------------------------------------------------------------
-- L-system による C 曲線
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


level =  10 :: Int
size  = 300 :: Float


main :: IO ()
main = runTurtle initDisp white 100 [(st, [cCurve level size])]
  where st  = initST {point = (-size / 2, size / 4), mark = False}


-- | C 曲線
cCurve :: Int -> Float -> Command
cCurve n size = l_system axiom rule n (size / (sqrt 2) ^ n) 45
  where
    axiom = "F"
    rule  = [('F', "-F++F-")]
