module Main where

import Graphics.Gloss
import Turtle
import L_system


main :: IO ()
main = display window white pic
    where
      window   = InWindow "Sandbox" (800, 600) (10, 10)
      pic = sandbox 6


sandbox :: Int -> Picture
sandbox n = drawLine st (size / 2 ^ n) 60 string
  where
    size = 250
    st   = initST {point = (size, - size * sqrt 3 / 2), angle = 180}
    axiom = "FXF--FF--FF"
    rules = [('F', "FF"), ('X', "--FXF++FXF++FXF--")]
    string = l_system axiom rules n

test n = l_system axiom rules n
  where
    axiom = "FXF--FF--FF"
    rules = [('F', "FF"), ('X', "--FXF++FXF++FXF--")]
