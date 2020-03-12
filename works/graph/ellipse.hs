------------------------------------------------------------
-- | 楕円のグラフを描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 20 [(initST, lst)]
  where
    lst = [grid 400, drawGraph' (ellipse 200 150) rose domain]
    domain = [0, 0.1 .. 2 * pi]


-- | 楕円のグラフ
ellipse :: Float -> Float -> (Float -> Float, Float -> Float)
ellipse a b = (\th -> a * cos th, \th -> b * sin th)
