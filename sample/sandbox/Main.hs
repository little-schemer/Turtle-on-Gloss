module Main where

import Graphics.Gloss
import Turtle
import L_system

import Debug.Trace


main :: IO ()
main = display window white $ pic1 <> pic2 <> pic4
    where
      window   = InWindow "Sandbox" (800, 600) (10, 10)
      pic1 = Color red $ Line [(-400, 0), (400, 0)]
      pic2 = Color red $ Line [(0, -400), (0, 400)]
      -- pic3 = snd $ runTurtle [left 30, fd 100, drawArcL 100 90] initST
      pic4 = Arc 90 370 100       -- start end 半径


-- drawArc :: (Float -> Command) -> Float -> Float -> Command
-- drawArc cmd r th st = (st, Translate ox oy $ Rotate (90 - angle st) $ Arc 0 th r)
--   where (ox, oy) = newPoint r $ fst (cmd 90 st)

-- drawArcL = drawArc left
-- drawArcR = drawArc right
