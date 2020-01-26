module Main where

import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window (greyN 0.3) 50 tLst
  where
    window = InWindow "Circle" (800, 600) (10, 10)
    col   = [setColor c | c <- cycle [red, green, blue]]
    angle = [setAngle th | th <- [30, 60 .. 360]]
    tLst  = [(initST, [a, b] ++ drawPolygonL 60 10) | (a, b) <- zip col angle]
