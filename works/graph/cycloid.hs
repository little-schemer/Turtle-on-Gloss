------------------------------------------------------------
-- | サイクロイド曲線を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 外サイクロイドまたは内サイクロイド
func :: (Float -> Float, Float -> Float)
func = epicycloid 100 85

-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 40 * pi]


-- Main
main :: IO ()
main = runTurtle window black 50 [(initST {penColor = cyan}, [cmd])]
  where
    window = initWindow {title = "Cycloid"}
    cmd = drawGraph' func rotationAngles


--
-- | 外サイクロイド epicycloid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + th : 回転角
--
--  x = (rc + rm) * cos th - rm * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rm * sin ((rc + rm) / rm * th)
--
epicycloid :: Float             -- ^ 定円の半径
           -> Float             -- ^ 動円の半径
           -> (Float -> Float, Float -> Float)
epicycloid rc rm = (fx, fy)
  where
    fx th = (rc + rm) * cos th - rm * cos ((rc + rm) / rm * th)
    fy th = (rc + rm) * sin th - rm * sin ((rc + rm) / rm * th)

--
-- | 内サイクロイド hypocycloid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + th : 回転角
--
--  x = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th - rm * sin ((rc - rm) / rm * th)
--
hypocycloid :: Float            -- ^ 定円の半径
            -> Float            -- ^ 動円の半径
            -> (Float -> Float, Float -> Float)
hypocycloid rc rm = (fx, fy)
  where
    fx th = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th - rm * sin ((rc - rm) / rm * th)
