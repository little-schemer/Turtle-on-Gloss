------------------------------------------------------------
-- 再帰による C 曲線
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


level =  10 :: Int
size  = 300 :: Float


main :: IO ()
main = runTurtle initDisp white 200 [(st, [cCurve level size])]
  where st  = initST {point = (-size / 2, size / 4), mark = False}


-- C 曲線
cCurve :: Int -> Float -> Command
cCurve 0 len = qf len
cCurve n len = concat [qr 45, cCurve', ql 90, cCurve', qr 45]
  where cCurve' = cCurve (n - 1) (len * sqrt 2 / 2)
