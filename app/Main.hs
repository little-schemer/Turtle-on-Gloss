module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window (greyN 0.3) [ (initST, lst) ]
  where
    window = InWindow "Circle" (800, 600) (10, 10)
    col = cycle [red, green, blue]
    lst = take 36 $ concat [setColor c : [drawPolygonL 60 10, rt 30] | c <- col]
