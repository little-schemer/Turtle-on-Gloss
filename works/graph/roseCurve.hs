------------------------------------------------------------
-- | 正葉曲線 (バラ曲線) を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window white 100 [(initST {penColor = rose}, lst)]
  where
    window = initWindow {title = "Rose Curve", zoom = 200}
    lst = [grid' 2 0.1, drawPolarGraph (roseCurve 5 2) [0, 0.01 .. pi * 4]]


--
-- | 正葉曲線 (バラ曲線)
--
--  r = sin (n / k * th)
--
--  refer to "https://sites.google.com/site/cinderellajapan/huanocg/huano-qu-xian"
--
roseCurve :: Float -> Float -> (Float -> Float)
roseCurve n k = (\th -> sin (n / k * th))
