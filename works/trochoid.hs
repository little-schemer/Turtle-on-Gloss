------------------------------------------------------------
-- トロコイド
------------------------------------------------------------


import           Graphics.Gloss
import           Graphics.Turtle


func :: Float -> Point
func = hypotrochoid 220 82 63

rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 1000]

main :: IO ()
main = runTurtle initDisp white 100 [tData]

tData :: (TurtleST, [Command])
tData = (st, map (\th -> goto (func th)) rotationAngles)
  where st = initST {point = func 0, mark = False}


--
-- 外トロコイド epitrochoid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + rd : 描画点の半径
--  + th : 回転角
--
--  x = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)
--
epitrochoid :: Float -> Float -> Float -> Float -> Point
epitrochoid rc rm rd th = (x, y)
  where
    x = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
    y = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)

--
-- 内トロコイド hypotrochoid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + rd : 描画点の半径
--  + th : 回転角
--
--  x = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
--
hypotrochoid :: Float -> Float -> Float -> Float -> Point
hypotrochoid rc rm rd th = (x, y)
  where
    x = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
    y = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
