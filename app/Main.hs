module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = display window white pic
  where
    window = InWindow "Turtle" (800, 600) (10, 10)
    pic    = Pictures $ snd $ runTurtle (0, (0, 0), black, True) cmds
    cmds   = take 250 $ concat [[forward n, right 93] | n <- iterate (* 1.05) 2]
