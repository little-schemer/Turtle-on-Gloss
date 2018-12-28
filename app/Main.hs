module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = do
  display window white pic
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      st = TurtleST {angle = 0, point = (0, 0), penColor = red, pen = True}
      cs = (left 9) : (concat $ replicate 12 [drawPolygonL 20 30, left 30])
      (_, pic) = runTurtle cs st
      -- (st', pic1) = drawPolygonL 20 100 st
      -- (_  , pic2) = drawPolygonR 20 100 (st' {penColor = blue})
