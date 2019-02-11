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
dragonCurve len n st = runTurtle (dR len n) st
  where
    dR len 0 = [fd len]
    dR len n = [lt 45] ++ dR len' n' ++ [rt 90] ++ dL len' n' ++ [lt 45]
      where (len', n') = (len / sqrt 2, n - 1)

    dL len 0 = [fd len]
    dL len n = [rt 45] ++ dR len' n' ++ [lt 90] ++ dL len' n' ++ [rt 45]
      where (len', n') = (len / sqrt 2, n - 1)
