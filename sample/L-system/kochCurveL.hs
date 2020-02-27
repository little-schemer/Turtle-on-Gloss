------------------------------------------------------------
-- | Koch 曲線 (L-system 版)
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


-- | パラメータ
level =   4 :: Int
size  = 400 :: Float


-- | L-system による Koch 曲線
kochCurve :: Int -> Float -> Command
kochCurve n size = l_system "F--F--F" [('F', "F+F--F+F")] n (size / 3^n) 60


-- | Main
main :: IO ()
main = runTurtle initDisp white 100 [(st1, cmd), (st2, cmd), (st3, cmd)]
  where
    st1  = initST {angle =    0, point = (-200,  200 / sqrt 3), mark = False}
    st2  = initST {angle = -120, point = ( 200,  200 / sqrt 3), mark = False}
    st3  = initST {angle =  120, point = (   0, -400 / sqrt 3), mark = False}
    cmd  = [kochCurve level size]
