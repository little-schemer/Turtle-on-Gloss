module Main where

import Graphics.Gloss
import Turtle


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
      window = InWindow "Turtle" (800, 600) (10, 10)
      (_, pic) = runTurtle (koch 400 4) initST { point = (-200, 0)}

      -- st = TurtleST {angle = 0, point = (0, 0), penColor = white, pen = True}
      -- cs = (left 9) : (concat $ replicate 12 [drawPolygonL 20 30, left 30])
      -- (_, pic) = runTurtle cs st
