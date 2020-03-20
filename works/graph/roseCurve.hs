------------------------------------------------------------
-- | 正葉曲線 (バラ曲線) を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 100 [(initST, lst)]
  where lst = [grid, drawPolarGraph (roseCurve 5 2) rose 200 [0, 0.01 .. pi * 4]]


-- | 極方程式のグラフを描く
drawPolarGraph :: (Float -> Float) -> Color -> Float -> [Float] -> Command
drawPolarGraph f c n domain = concat $ cmd1 ++ cmd2
  where
    cmd1 = let th = head domain in [pu, goto (polarToRectangular (n * f th, th)), pd]
    cmd2 = setColor c : [goto (polarToRectangular (n * f th, th)) | th <- domain]


--
-- | 正葉曲線 (バラ曲線)
--
--  r = sin (n / k * th)
--
--  refer to "https://sites.google.com/site/cinderellajapan/huanocg/huano-qu-xian"
--
roseCurve :: Float -> Float -> (Float -> Float)
roseCurve n k = (\th -> sin (n / k * th))
