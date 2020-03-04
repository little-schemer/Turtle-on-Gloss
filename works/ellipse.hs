------------------------------------------------------------
-- | 楕円のグラフを描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 楕円のグラフ
ellipse :: Float -> Float -> Color -> Command
ellipse a b c = drawGraph' fx fy c [0, 0.1 .. 2 * pi]
  where
    fx th = a * cos th
    fy th = b * sin th


-- | Main
main :: IO ()
main = runTurtle initDisp white 20 [(st, lst)]
  where
    st  = initST {mark = False}
    lst = [grid, ellipse 200 100 rose]
