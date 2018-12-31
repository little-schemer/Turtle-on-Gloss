module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = display window black (Pictures [pic1, pic2, pic3, pic4])
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      (_, pic1) = dragonCurve 200 10 initST {penColor = red                }
      (_, pic2) = dragonCurve 200 10 initST {penColor = green,  angle =  90}
      (_, pic3) = dragonCurve 200 10 initST {penColor = blue,   angle = 180}
      (_, pic4) = dragonCurve 200 10 initST {penColor = yellow, angle = 270}


-- | 再帰関数によるドラゴン曲線
dragonCurve :: Float -> Int -> Command
dragonCurve len n st = runTurtle (dragonR len n) st
  where
    dragonR len 0 = [forward len]
    dragonR len n = lt ++ dR ++ rt ++ dL ++ lt
      where
        lt = [left 45]
        rt = [right 90]
        dR = dragonR (len / sqrt 2) (n - 1)
        dL = dragonL (len / sqrt 2) (n - 1)

    dragonL len 0 = [forward len]
    dragonL len n = rt ++ dR ++ lt ++ dL ++ rt
      where
        lt = [left 90]
        rt = [right 45]
        dR = dragonR (len / sqrt 2) (n - 1)
        dL = dragonL (len / sqrt 2) (n - 1)
