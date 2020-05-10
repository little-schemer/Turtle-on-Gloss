------------------------------------------------------------
-- | サイクロイド曲線を描く
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | drawGraph' に与える関数
func = epicycloid 100 85
-- func = astroid 200
-- func = cardioid 100


-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 40 * pi]


-- Main
main :: IO ()
main = runTurtle window black 50 [(initST {penColor = cyan}, [grid, cmd])]
  where
    window = initWindow {title = "Cycloid"}
    cmd    = drawGraph' func rotationAngles


--
-- | 外サイクロイド epicycloid
--
--
--  + 定円の半径   : rc
--  + 動円の半径   : rm
--  + 回転角       : th
--
--  x = (rc + rm) * cos th - rm * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rm * sin ((rc + rm) / rm * th)
--
epicycloid :: Float             -- ^ rc
           -> Float             -- ^ rm
           -> (Float -> Float, Float -> Float)
epicycloid rc rm = (fx, fy)
  where
    fx th = (rc + rm) * cos th - rm * cos ((rc + rm) / rm * th)
    fy th = (rc + rm) * sin th - rm * sin ((rc + rm) / rm * th)

--
-- | 内サイクロイド hypocycloid
--
--  + 定円の半径   : rc
--  + 動円の半径   : rm
--  + 回転角       : th
--
--  x = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th - rm * sin ((rc - rm) / rm * th)
--
hypocycloid :: Float            -- ^ rc
            -> Float            -- ^ rm
            -> (Float -> Float, Float -> Float)
hypocycloid rc rm = (fx, fy)
  where
    fx th = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th - rm * sin ((rc - rm) / rm * th)



-- 特殊なサイクロイド ----------------------------

-- | Astroid
astroid :: Float -> (Float -> Float, Float -> Float)
astroid r = hypocycloid r (r / 4)

-- | Cardioid
cardioid :: Float -> (Float -> Float, Float -> Float)
cardioid r = epicycloid r r
