module Main where

import           Graphics.Gloss
import           Turtle


title = "Circle"
size  = (800, 600)
pos   = (0, 0)
back  = white

main :: IO ()
main = runTurtle (title, size, pos, back) [ (initST, lst) ]
  where
    col = cycle [red, green, blue]
    lst = take 36 $ concat [setColor c : [drawPolygonL 60 10, rt 30] | c <- col]
