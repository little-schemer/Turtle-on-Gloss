------------------------------------------------------------
-- | Circle Dance
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 色を変化させながら、複数の円を同時に描く
circleDance :: Command
circleDance = repCommand 6 [circle', lt 10]
  where
    circle' = repCommand 36 [drawArcL 10 100, updateColor f f f id]
    f x = x * 1.015


-- | 色と初期角度の設定
colorAndAngle :: [(Color, Float)]
colorAndAngle = zip [c1, c2, c3, c4, c5, c6] [30, 90 .. 360]
  where
    c1 = makeColor 0.02 0.02 0.10 1
    c2 = makeColor 0.02 0.10 0.10 1
    c3 = makeColor 0.02 0.10 0.02 1
    c4 = makeColor 0.10 0.10 0.02 1
    c5 = makeColor 0.10 0.02 0.02 1
    c6 = makeColor 0.10 0.02 0.10 1


-- | Main
main :: IO ()
main = runTurtle windows black 50 (zip (repeat initST) cmds)
  where
    windows = InWindow "Circle Dance" (800, 600) (10, 10)
    cmds = [[setColor a, setAngle b, circleDance] | (a, b) <- colorAndAngle]
