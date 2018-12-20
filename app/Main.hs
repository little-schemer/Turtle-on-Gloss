module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = display window white pic
  where
    window = InWindow "Turtle" (800, 600) (10, 10)
    pic    = Pictures $ snd $ runTurtle (0, (0, 0), black, True) pList
    cmds   = [[Forward n, TurnRight 92] | n <- iterate (* 1.05) 5]
    color  = [SetColor (makeColorI r g 255 255) | r <- [5, 10 .. 255], g <- [255, 250 .. 5]]
    pList  = take 500 $ concat $ zipWith (:) color cmds
