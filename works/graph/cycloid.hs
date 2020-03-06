------------------------------------------------------------
-- | サイクロイド曲線を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | 外サイクロイドまたは内サイクロイド
cmd :: Command
cmd = epicycloid 100 85 cyan rotationAngles

-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 40 * pi]


-- Main
main :: IO ()
main = runTurtle initDisp black 50 [(initST, [cmd])]


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
           -> Color             -- ^ 線の色
           -> [Float]           -- ^ 回転角のリスト
           -> Command
epicycloid rc rm c domain = drawGraph' fx fy c domain
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
--  y = (rc - rm) * sin th + rm * sin ((rc - rm) / rm * th)
--
hypocycloid :: Float            -- ^ 定円の半径
            -> Float            -- ^ 動円の半径
            -> Color            -- ^ 線の色
            -> [Float]          -- ^ 回転角のリスト
            -> Command
hypocycloid rc rm c domain = drawGraph' fx fy c domain
  where
    fx th = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th + rm * sin ((rc - rm) / rm * th)
