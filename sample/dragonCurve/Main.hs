module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window (greyN 0.3) [(s, cmd) | s <- [st1, st2, st3, st4]]
  where
    window = InWindow "Dragon Curve" (800, 600) (10, 10)
    cmd = [dragonCurve 200 10]
    st1 = initST { penColor = red }
    st2 = initST { penColor = green,  angle =  90 }
    st3 = initST { penColor = blue,   angle = 180 }
    st4 = initST { penColor = yellow, angle = 270 }


-- | 再帰関数によるドラゴン曲線
dragonCurve :: Float -> Int -> Command
dragonCurve len n st = concatCmd (dR len n) st
  where
    dR len 0 = [fd len]
    dR len n = [lt 45] ++ dR len' n' ++ [rt 90] ++ dL len' n' ++ [lt 45]
      where (len', n') = (len / sqrt 2, n - 1)

    dL len 0 = [fd len]
    dL len n = [rt 45] ++ dR len' n' ++ [lt 90] ++ dL len' n' ++ [rt 45]
      where (len', n') = (len / sqrt 2, n - 1)
