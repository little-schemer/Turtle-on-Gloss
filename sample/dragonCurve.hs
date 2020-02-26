------------------------------------------------------------
-- |
--   Module    : DragonCurve.hs
--   Copyright : (c) little Haskeller
--   License   : BSD3
--
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


level =  10 :: Int
size  = 300 :: Float


-- | 再帰による Dragon 曲線
dragonCurve :: Int -> Float -> Command
dragonCurve n len = dR n len
  where
    dR 0 len = qf len
    dR n len = concat [ql 45, dR n' len', qr 90, dL n' len', ql 45]
      where (n', len') = (n - 1, len / sqrt 2)

    dL 0 len = qf len
    dL n len = concat [qr 45, dR n' len', ql 90, dL n' len', qr 45]
      where (n', len') = (n - 1, len / sqrt 2)


main :: IO ()
main = runTurtle window (greyN 0.3) 100 [(s, cmd) | s <- [st1, st2, st3, st4]]
  where
    window = InWindow "Dragon Curve" (800, 800) (10, 10)
    st1 = initST {mark = False, penColor = red,    angle =  45}
    st2 = initST {mark = False, penColor = green,  angle = 135}
    st3 = initST {mark = False, penColor = blue,   angle = 225}
    st4 = initST {mark = False, penColor = yellow, angle = 315}
    cmd = [dragonCurve level size]
