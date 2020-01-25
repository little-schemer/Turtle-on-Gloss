module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window (greyN 0.3) 100 [(s, cmd) | s <- [st1, st2, st3, st4]]
  where
    window = InWindow "Dragon Curve" (800, 600) (10, 10)
    cmd = [dragonCurve 200 10]
    st1 = initST { mark = False, penColor = red }
    st2 = initST { mark = False, penColor = green,  angle =  90 }
    st3 = initST { mark = False, penColor = blue,   angle = 180 }
    st4 = initST { mark = False, penColor = yellow, angle = 270 }


-- | 再帰関数によるドラゴン曲線
dragonCurve :: Float -> Int -> Command
dragonCurve len n = dR len n
  where
    dR len 0 = qf len
    dR len n = ql 45 ++ dR len' n' ++ qr 90 ++ dL len' n' ++ ql 45
      where (len', n') = (len / sqrt 2, n - 1)

    dL len 0 = qf len
    dL len n = qr 45 ++ dR len' n' ++ ql 90 ++ dL len' n' ++ qr 45
      where (len', n') = (len / sqrt 2, n - 1)
