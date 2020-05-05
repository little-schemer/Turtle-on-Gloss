------------------------------------------------------------
-- | 関数のグラフを描く
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | 定義域
domain :: [Float]
domain = [-50.0, -49.9 .. 50.0]

-- | 二次関数の例
graph1 :: Command
graph1 = drawGraph f domain
  where f x = (1 / 8) * x ^ 2 - 2 * x - 5

-- | 一次関数の例
graph2 :: Command
graph2 = drawGraph f domain
  where f x = (-1) / 3 * x - 5


-- Main
main :: IO ()
main = runTurtle disp white 200 [tData0, tData1, tData2]
  where
    disp   = initWindow {title = "Graph", zoom = 10, shiftXY = (0, -10)}
    st     = initST
    tData0 = (st, [grid' 40 1])
    tData1 = (st, [setColor rose,  graph1])
    tData2 = (st, [setColor azure, graph2])
