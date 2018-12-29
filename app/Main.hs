module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window black pic
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      st = TurtleST {angle = 0, point = (0, 0), penColor = white, pen = True}
      cs = (left 9) : (concat $ replicate 12 [drawPolygonL 20 30, left 30])
      (_, pic) = runTurtle cs st
