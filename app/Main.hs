module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
    where window = InWindow "Triangle" (800, 600) (10, 10)



-- (pic, _) = runTurtle cmdLst initST
--   where cmdLst = [forward 100, left 120, forward 100, left 120, forward 100]

(pic, _) = runTurtle cmdLst initST
  where cmdLst = [forward 100] ++ [drawPolygonL 10 50] ++ [forward 100]
