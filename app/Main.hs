module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
    where
      window = InWindow "Triangle" (800, 600) (10, 10)
      cmdLst = [forward 100, left 120, forward 100, left 120, forward 100]
      (_, pic) = runTurtle cmdLst initST
