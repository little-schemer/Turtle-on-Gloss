------------------------------------------------------------
-- | 楕円のグラフを描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 20 [(initST, lst)]
  where lst = [grid, ellipse 200 100 rose]


-- | 楕円のグラフ
ellipse :: Float -> Float -> Color -> Command
ellipse a b c = drawGraph' fx fy c [0, 0.1 .. 2 * pi]
  where
    fx th = a * cos th
    fy th = b * sin th
