module Main where

import Graphics.Gloss
import Turtle


main :: IO ()
main = display window black (Pictures [pic1, pic2, pic3, pic4])
    where
      window = InWindow "Dragon Curve" (800, 600) (10, 10)
      pic1 = fst $ dragonCurve 200 10 initST {penColor = red                }
      pic2 = fst $ dragonCurve 200 10 initST {penColor = green,  angle =  90}
      pic3 = fst $ dragonCurve 200 10 initST {penColor = blue,   angle = 180}
      pic4 = fst $ dragonCurve 200 10 initST {penColor = yellow, angle = 270}


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
