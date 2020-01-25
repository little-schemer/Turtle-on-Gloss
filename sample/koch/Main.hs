module Main where

import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 500 [(st, lst)]
  where
    window = InWindow "Koch Curve" (800, 600) (10, 10)
    st  = initST { point = (-200, 200 / sqrt 3), mark = False }
    lst = take 5 $ cycle [kochCurve 400 4, rt 120]


-- | 再帰関数によるコッホ曲線
kochCurve :: Float -> Int -> Command
kochCurve len n = kh len n
  where
    kh len 0 = qf len
    kh len n = kh' ++ ql 60 ++ kh' ++ qr 120 ++ kh' ++ ql 60 ++ kh'
      where kh' = kh (len / 3) (n - 1)
