------------------------------------------------------------
-- | 色を変えながら螺旋を描く
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window black 100 [(st, spiral2 300)]
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    st = initST {point = (-1, 1), penColor = initColor}
    initColor = makeColor 0 1 0.5 0.1


-- | 螺旋を描く
spiral :: Int -> [Command]
spiral n = concat $ take n [[fd len, updateColor f1 f1 id f2, qr 92] | len <- nLst]
  where
    f1 x = if x < 1 then 1 else 0
    f2 x = x * 1.02
    nLst = iterate (* 1.02) 2.0

-- | 螺旋を描く
spiral2 :: Int -> [Command]
spiral2 n = loop n 2.0
  where
    loop 0 _    = []
    loop n size = makeCmd size ++ loop (n - 1) (size * 1.02)
    makeCmd size = [fd size, updateColor f1 f1 id f2, rt 92]
    f1 x = if x < 1 then 1 else 0
    f2 x = x * 1.02
