module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window white [(st, lst)]
  where
    window = InWindow "Koch Curve" (800, 600) (10, 10)
    st  = initST { point = (-200, 200 / sqrt 3) }
    lst = take 5 $ cycle [kochCurve 400 4, rt 120]


-- | 再帰関数によるコッホ曲線
kochCurve :: Float -> Int -> Command
kochCurve len n st = concatCmd (kh len n) st
  where
    kh len 0 = [fd len]
    kh len n = kh' ++ [lt 60] ++ kh' ++ [rt 120] ++ kh' ++ [lt 60] ++ kh'
      where kh' = kh (len / 3) (n - 1)
