------------------------------------------------------------
-- | トロコイド曲線を描く
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | drawGraph' に与える関数
func :: (Float -> Float, Float -> Float)
func = hypotrochoid 220 85 60
-- func = epitrochoid 100 85 110


-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.05 .. 40 * pi]


-- | Main
main :: IO ()
main = runTurtle window white 30 [(initST {penColor = rose}, [cmd])]
  where
    window = initWindow {title = "Trochoid"}
    cmd    = drawGraph' func rotationAngles


--
-- | 外トロコイド epitrochoid
--
--  + 定円の半径   : rc
--  + 動円の半径   : rm
--  + 描画点の半径 : rd
--  + 回転角       : th
--
--  x = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)
--
epitrochoid :: Float            -- ^ rc
            -> Float            -- ^ rm
            -> Float            -- ^ rd
            -> (Float -> Float, Float -> Float)
epitrochoid rc rm rd = (fx, fy)
  where
    fx th = (rc + rm) * cos th - rd * cos ((rc + rm) / rm * th)
    fy th = (rc + rm) * sin th - rd * sin ((rc + rm) / rm * th)

--
-- | 内トロコイド hypotrochoid
--
--  + 定円の半径   : rc
--  + 動円の半径   : rm
--  + 描画点の半径 : rd
--  + 回転角       : th
--
--  x = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
--
hypotrochoid :: Float           -- ^ rc
             -> Float           -- ^ rm
             -> Float           -- ^ rd
             -> (Float -> Float, Float -> Float)
hypotrochoid rc rm rd = (fx, fy)
  where
    fx th = (rc - rm) * cos th + rd * cos ((rc - rm) / rm * th)
    fy th = (rc - rm) * sin th - rd * sin ((rc - rm) / rm * th)
