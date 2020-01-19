module Main where

import           Graphics.Gloss
import           Turtle

main :: IO ()
main = runTurtle ("Triangle", (800, 600)) [(initST, lst)]
  where lst = (take 5 $ cycle [fd 150, left 120])


-- main = display window white pic
--     where
--       window = InWindow "Triangle" (800, 600) (10, 10)
--       (pic, _) = concatCmd (take 5 $ cycle [forward 150, left 120]) initST
