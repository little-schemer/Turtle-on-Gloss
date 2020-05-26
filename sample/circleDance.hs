------------------------------------------------------------
-- | Circle Dance
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window black 50 (zip (repeat initST) cmds)
  where
    window = initWindow {title = "Circle Dance"}
    cmds   = [[setColor a, setAngle b, circleDance] | (a, b) <- colorAndAngle]

-- | 色を変化させながら、複数の円を同時に描く
circleDance :: Command
circleDance = repCommand 6 [repCommand 36 cmds, lt 10]
  where
    cmds = [drawArcL 10 100, updateColor f f f id, updateThickness (+ 0.05)]
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
