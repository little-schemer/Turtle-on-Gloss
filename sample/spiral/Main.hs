module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    (_, pic) = spiral 230 initST {penColor = blue}


-- | らせん
spiral :: Int -> Command
spiral n st = runTurtle (take n $ cmdLst) st
  where cmdLst = concat [[forward len, right 93] | len <- iterate (* 1.05) 2]
