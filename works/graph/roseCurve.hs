------------------------------------------------------------
-- | 正葉曲線 (バラ曲線) を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 100 [(initST {penColor = rose}, lst)]
  where lst = [grid, drawPolarGraph (roseCurve 200 5 2) [0, 0.01 .. pi * 4]]


--
-- | 正葉曲線 (バラ曲線)
--
--  r = m * sin (n / k * th)
--
--  refer to "https://sites.google.com/site/cinderellajapan/huanocg/huano-qu-xian"
--
roseCurve :: Float -> Float -> Float -> (Float -> Float)
roseCurve m n k = (\th -> m * sin (n / k * th))
