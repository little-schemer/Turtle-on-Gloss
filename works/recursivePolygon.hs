------------------------------------------------------------
-- | 再帰的多角形
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


level =   6 :: Int
size  = 500 :: Float
divN  =   2 :: Float
n     =   3 :: Int              -- 角数



-- | Main
main :: IO ()
main = runTurtle window white 100 [(st, lst)]
  where
    window = initWindow
    st     = initST {point = (-size / 2, -size / 2), mark = False}
    lst    = [recursivePolygon n level size]



recursivePolygon :: Int -> Int -> Float -> Command
recursivePolygon n lev size = cmd lev size
  where
    n' = fromIntegral n
    angle = 360 / n'
    cmd 1   size = repCommand n [qf size, ql angle]
    cmd lev size = repCommand n [cmd (lev - 1) (size / divN), qf size, ql angle]
