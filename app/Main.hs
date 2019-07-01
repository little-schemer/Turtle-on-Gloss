module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
    where
      window = InWindow "Triangle" (800, 600) (10, 10)
      (pic, _) = runTurtle (take 5 $ cycle [forward 150, left 120]) initST
