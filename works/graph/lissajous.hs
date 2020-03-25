------------------------------------------------------------
-- | リサージュ曲線
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | drawGraph' に与える関数
func :: (Float -> Float, Float -> Float)
func = lissajous (1, 1) (3, 4) 0


-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.05 .. 10 * pi]


-- | Main
main :: IO ()
main = runTurtle window black 50 [(st, [grid' 10 0.1, cmd])]
  where
    st     = initST {penColor = green}
    window = initWindow {title = "リサージュ曲線", zoom = 200}
    cmd    = drawGraph' func rotationAngles


--
-- | リサージュ曲線 Lissajous Curve
--
--  + 振幅     : A, B
--  + 角周波数 : a, b
--  + 位相差   : δ
--  + 回転角   : θ
--
--  x = A * sin (a * θ + δ)
--  y = B * sin (b * θ)
--
lissajous :: (Float, Float)     -- ^ (A, B)
          -> (Float, Float)     -- ^ (a, b)
          -> Float              -- ^ δ
          -> (Float -> Float, Float -> Float)
lissajous (m, n) (a, b) d = (fx, fy)
  where
    fx th = m * sin (a * th + d)
    fy th = n * sin (b * th)
