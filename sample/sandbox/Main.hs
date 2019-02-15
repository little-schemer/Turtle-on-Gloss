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
      pic3 = snd $ runTurtle [left 300, fd 100, drawArcL 90 100, lt 90, drawArcL 90 100] initST
      -- pic4 = Arc 90 370 100       -- start end 半径

-- drawArcL :: Float -> Float -> Command
-- drawArcL r th st = (st', Translate ox oy $ Rotate (180 - a - th) $ isDraw st $ Arc 0 th r)
--   where
--     a = angle st
--     (ox, oy) = newPoint r (a + 90) (point st)
--     st' = st {angle = a + th, point = newPoint r (a + th - 90) (ox, oy)}

-- drawArcR :: Float -> Float -> Command
-- drawArcR r th st = (st', Translate ox oy $ Rotate (th - a - 90) $ isDraw st $ Arc 0 th r)
--   where
--     a = angle st
--     (ox, oy) = newPoint r (a - 90) (point st)
--     st' = st {angle = a - th, point = newPoint r (a - th + 90) (ox, oy)}




drawArcL :: Float -> Float -> Command
drawArcL th r st = (st', Translate ox oy $ isDraw st $ Arc a' (a' + th) r)
  where
    a  = angle st
    a' = a - 90
    (ox, oy) = newPoint r (a + 90) (point st)
    st' = st {angle = a + th, point = newPoint r (a + th - 90) (ox, oy)}

drawArcR :: Float -> Float -> Command
drawArcR th r st = (st', Translate ox oy $ isDraw st $ Arc a' (a' + th) r)
  where
    a  = angle st
    a' = 90 + a - th
    (ox, oy) = newPoint r (a - 90) (point st)
    st' = st {angle = a - th, point = newPoint r (a - th + 90) (ox, oy)}
