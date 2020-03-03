------------------------------------------------------------
-- | 関数のグラフを描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 値域
range :: [Float]
range = [-400.0, -399.5 .. 400.0]

-- | 二次関数の例
graph1 :: Command
graph1 = drawGraph f rose range
  where f x = 1 / 50 * x ^ 2 - 2 * x - 100

-- | 一次関数の例
graph2 :: Command
graph2 = drawGraph f azure range
  where f x = (-1) / 3 * x - 50


-- Main
main :: IO ()
main = runTurtle disp white 500 [tData0, tData1, tData2]
  where
    disp   = InWindow "Graph" (800, 600) (10, 10)
    st     = initST {mark = False}
    tData0 = (st, [grid])
    tData1 = (st, [graph1])
    tData2 = (st, [graph2])
