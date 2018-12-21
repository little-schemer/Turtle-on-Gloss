module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    pic  = Pictures $ snd $ runTurtle iST $ (star 100) ++ [forward 100]
    iST  = (0, (0, 0), black, True)


-- star :: Float -> TurtleST -> (TurtleST, Picture)
star n = concat $ replicate 5 [forward n, right 144]
