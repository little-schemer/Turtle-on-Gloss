------------------------------------------------------------
-- | トロコイド曲線を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 外トロコイドまたは内トロコイド
cmd :: Command
cmd = hypotrochoid 220 85 60 rose rotationAngles

-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 40 * pi]


-- | Main
main :: IO ()
main = runTurtle initDisp white 50 [(initST, [cmd])]


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
            -> Color            -- ^ 線の色
            -> [Float]          -- ^ 回転角のリスト
            -> Command
epitrochoid rc rm rd c domain = drawGraph' fx fy c domain
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
             -> Color           -- ^ 線の色
             -> [Float]         -- ^ 回転角のリスト
             -> Command
hypotrochoid rc rm rd c domain = drawGraph' fx fy c domain
  where
    fx th = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
