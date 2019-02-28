module Main where

import Graphics.Gloss
import Turtle
import L_system

import Debug.Trace


main :: IO ()
main = display window white $ pic1 <> pic2 <> pic3
    where
      window   = InWindow "Sandbox" (800, 600) (10, 10)
      pic1 = Color red $ Line [(-400, 0), (400, 0)]
      pic2 = Color red $ Line [(0, -400), (0, 400)]
      pic3 = fst $ runTurtle (take 18 cmdLst) initST


      cLst = [red, yellow, violet, blue, cyan, aquamarine, chartreuse]
      cmdLst = loop cLst
        where loop (c : cs) = [setColor c, drawArcL 360 100, lt 60] ++ loop cs
