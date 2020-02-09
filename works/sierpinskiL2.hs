------------------------------------------------------------
-- L-system による Sierpinski の三角形
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


level =  10 :: Int
size  = 500 :: Float


main :: IO ()
main = dispPicture initDisp white [(st, [sierpinski level size])]
  where st = initST {point = (-size / 2, -size / (2 * sqrt 2)), mark = False}


-- Sierpinski の三角形
sierpinski :: Int -> Float -> Command
sierpinski n size = l_system axiom rule n (size / 2^(n - 1)) 120
  where
    axiom = "F"
    rule  = [('F', "F+F+F+ff"), ('f', "ff")]
