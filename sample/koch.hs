------------------------------------------------------------
-- 再帰による Koch 曲線
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


level =   4 :: Int
size  = 400 :: Float


main :: IO ()
main = runTurtle window white 100 [(st1, cmd), (st2, cmd), (st3, cmd)]
  where
    window = InWindow "Koch Curve" (800, 600) (10, 10)
    st1 = initST {angle =    0, point = (-200,  200 / sqrt 3), mark = False}
    st2 = initST {angle = -120, point = ( 200,  200 / sqrt 3), mark = False}
    st3 = initST {angle =  120, point = (   0, -400 / sqrt 3), mark = False}
    cmd = [kochCurve level size]


-- | 再帰関数によるコッホ曲線
kochCurve :: Int -> Float -> Command
kochCurve 0 len = qf len
kochCurve n len = concat [kh, ql 60, kh, qr 120, kh, ql 60, kh]
  where kh = kochCurve (n - 1) (len / 3)
