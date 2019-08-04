module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
    where
      window = InWindow "Test" (800, 600) (10, 10)
      (pic, _) = runTurtle [fd 150, drawCircle 100] initST
