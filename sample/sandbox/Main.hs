module Main where

import Graphics.Gloss
import Turtle
import L_system

import Debug.Trace


main :: IO ()
main = display window white $ pic1 <> pic2
    where
      window   = InWindow "Sandbox" (800, 600) (10, 10)
      pic1 = Color red $ Line [(-400, 0), (400, 0)]
      pic2 = Color red $ Line [(0, -400), (0, 400)]
