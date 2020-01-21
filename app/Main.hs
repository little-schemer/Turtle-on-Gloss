module Main where

import           Graphics.Gloss
import           Turtle

main :: IO ()
main = runTurtle window (greyN 0.3) [(initST, [fd 200])]
  where window = InWindow "test" (800, 600) (10, 10)
