------------------------------------------------------------
-- | Dragon 曲線 (L-system 版)
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


-- | パラメータ
level =  10 :: Int
size  = 300 :: Float


-- | Main
main :: IO ()
main = runTurtle window (greyN 0.3) 100 [(s, cmd) | s <- [st1, st2, st3, st4]]
  where
    window = initWindow {title = "Dragon Curve by L-system"}
    st1 = initST {mark = False, penColor = red,    angle =  45}
    st2 = initST {mark = False, penColor = green,  angle = 135}
    st3 = initST {mark = False, penColor = blue,   angle = 225}
    st4 = initST {mark = False, penColor = yellow, angle = 315}
    cmd = [dragonCurve level size]


-- | L-system による Dragon 曲線
dragonCurve :: Int -> Float -> Command
dragonCurve n size = l_system axiom rule n (size / (sqrt 2)^n) 90
  where
    axiom = "FX"
    rule  = [('X', "X+YF+"), ('Y', "-FX-Y")]
