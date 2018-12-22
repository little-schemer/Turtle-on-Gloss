module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = do
  display window white pic
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      cmdLst = concat $ replicate 1 [circleL 100, left 40]
      pic    = snd $ runTurtle cmdLst (0, (0, 0), red, True)
