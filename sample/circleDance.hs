------------------------------------------------------------
-- 色を変化させながら、複数の円を同時に描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


main :: IO ()
main = runTurtle windows black 50 tLst
  where windows = InWindow "Circle Dance" (800, 600) (10, 10)

cols :: [Command]
cols = [setColor c | c <- cycle [col1, col2, col3, col4, col5, col6]]
  where
    col1 = makeColor 0.0 0.0 0.1 1
    col2 = makeColor 0.0 0.1 0.1 1
    col3 = makeColor 0.0 0.1 0.0 1
    col4 = makeColor 0.1 0.1 0.0 1
    col5 = makeColor 0.1 0.0 0.0 1
    col6 = makeColor 0.1 0.0 0.1 1

angles :: [Command]
angles = [setAngle th | th <- [30, 90 .. 360]]

circleDance :: Command
circleDance = repCommand 6 [circle', lt 10]
  where
    circle' = repCommand 36 [drawArcL 10 100, updateColor f f f id]
    f x = x * 1.012

tLst :: [(TurtleST, [Command])]
tLst = [(initST, [a, b] ++ [circleDance]) | (a, b) <- zip cols angles]
