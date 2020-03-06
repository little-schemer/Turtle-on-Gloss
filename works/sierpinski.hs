------------------------------------------------------------
-- | Sierpinski の三角形 (再帰版)
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | パラメータ
level =   8 :: Int
size  = 500 :: Float


-- | Main
main :: IO ()
main = runTurtle initDisp white 500 [(st, [sierpinski level size])]
  where st = initST {point = (-size / 2, -size / (2 * sqrt 2)), mark = False}


-- | 再帰による Sierpinski の三角形
sierpinski :: Int -> Float -> Command
sierpinski n len = if odd n
                   then concat [ql 60, sierA n len]
                   else sierA n len
  where
    sierA 0 len = qf len
    sierA n len = concat [ sierB (n - 1) (len / 2)
                         , qr 60
                         , sierA (n - 1) (len / 2)
                         , qr 60
                         , sierB (n - 1) (len / 2)
                         ]

    sierB 0 len = qf len
    sierB n len = concat [ sierA (n - 1) (len / 2)
                         , ql 60
                         , sierB (n - 1) (len / 2)
                         , ql 60
                         , sierA (n - 1) (len / 2)
                         ]
