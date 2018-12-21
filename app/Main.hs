module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = do
  display window white $ Pictures [pic1, pic2]
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      (_, pic1) = circleR 100 (0, (0, 0), red, True)
      (_, pic2) = circleL 100 (0, (0, 0), blue, True)
