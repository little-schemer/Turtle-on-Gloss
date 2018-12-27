module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = do
  display window white pic
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      st = TurtleST {angle = 0, point = (0, 0), penColor = red, pen = True}
      (st', pic) = drawPolygon 20 100 st
