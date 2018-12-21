module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = do
  display window white $ Pictures [pic1, pic2, Line [(0, 200), (0, -200)], Line [(-200, 0), (200, 0)]]
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      (_, pic1) = circleR 100 (0, (0, 0), red, True)
      (_, pic2) = circleL 100 (0, (0, 0), blue, True)
    -- cmds = concat $ replicate 36 [forward 50, right 10]
    -- pic = snd $ runTurtle cmds (0, (0, 0), red, True)
