--
-- | 再帰によるコッホ曲線
--

module Main where

import Graphics.Gloss
import Turtle

-- | コッホ曲線
koch :: Float -> Int -> [Command]
koch len 0 = [forward len]
koch len n = koch' ++ lt ++ koch' ++ rt ++ koch' ++ lt ++ koch'
  where
    koch' = koch (len / 3) (n - 1)
    lt    = [left 60]
    rt    = [right 120]


main :: IO ()
main = display window white pic
    where
      window = InWindow "Koch Curve" (800, 600) (10, 10)
      (_, pic) = runTurtle (koch 400 4) initST { point = (-200, 0)}
