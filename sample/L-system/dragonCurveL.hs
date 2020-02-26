------------------------------------------------------------
-- |
--   Module    : DragonCurveL
--   Copyright : (c) little Haskeller
--   License   : BSD3
--
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


level =  10 :: Int
size  = 400 :: Float


-- | L-system による Dragon 曲線
dragonCurve :: Int -> Float -> Command
dragonCurve n size = l_system axiom rule n (size / (sqrt 2)^n) 90
  where
    axiom = "FX"
    rule  = [('X', "X+YF+"), ('Y', "-FX-Y")]


main :: IO ()
main = runTurtle window (greyN 0.3) 100 [(s, cmd) | s <- [st1, st2, st3, st4]]
  where
    window = InWindow "Dragon Curve" (800, 800) (10, 10)
    st1 = initST {mark = False, penColor = red,    angle =  45}
    st2 = initST {mark = False, penColor = green,  angle = 135}
    st3 = initST {mark = False, penColor = blue,   angle = 225}
    st4 = initST {mark = False, penColor = yellow, angle = 315}
    cmd = [dragonCurve level size]
