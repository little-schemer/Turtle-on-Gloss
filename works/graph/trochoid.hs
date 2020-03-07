------------------------------------------------------------
-- | トロコイド曲線を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 外トロコイドまたは内トロコイド
func :: (Float -> Float, Float -> Float)
func = hypotrochoid 220 85 60

-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 40 * pi]


-- | Main
main :: IO ()
main = runTurtle initDisp white 50 [(initST, [cmd])]
  where cmd = drawGraph' func rose rotationAngles


--
-- | 外トロコイド epitrochoid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + rd : 描画点の半径
--  + th : 回転角
--
--  x = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)
--
epitrochoid :: Float            -- ^ 定円の半径
            -> Float            -- ^ 動円の半径
            -> Float            -- ^ 描画点の半径
            -> (Float -> Float, Float -> Float)
epitrochoid rc rm rd = (fx, fy)
  where
    fx th = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
    fy th = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)

--
-- | 内トロコイド hypotrochoid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + rd : 描画点の半径
--  + th : 回転角
--
--  x = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
--
hypotrochoid :: Float           -- ^ 定円の半径
             -> Float           -- ^ 動円の半径
             -> Float           -- ^ 描画点の半径
             -> (Float -> Float, Float -> Float)
hypotrochoid rc rm rd = (fx, fy)
  where
    fx th = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
