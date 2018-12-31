module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = display window white pic
    where
      window   = InWindow "Koch Curve" (800, 600) (10, 10)
      (kc, rt) = (kochCurve 400 4, right 120)
      st       = initST {point = (-200, 200 / sqrt 3)}
      (_, pic) = runTurtle [kc, rt, kc, rt, kc] st


-- | 再帰関数によるコッホ曲線
kochCurve :: Float -> Int -> Command
kochCurve len n st = runTurtle (koch len n) st
  where
    koch len 0 = [forward len]
    koch len n = koch' ++ lt ++ koch' ++ rt ++ koch' ++ lt ++ koch'
      where (koch', lt, rt) = (koch (len / 3) (n - 1), [left 60], [right 120])
