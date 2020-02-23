import Graphics.Gloss
import Graphics.Turtle


main :: IO ()
main = runTurtle window black 200 [(st, spiral 300)]
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    st = initST {point = (-1, 1), penColor = initColor, mark = False}
    initColor = makeColor 0 1 0.5 0.1

spiral :: Int -> [Command]
spiral n = concat $ take n [[fd len, updateColor f1 f1 id f2, qr 92] | len <- nLst]
  where
    f1 x = if x < 1 then 1 else 0
    f2 x = x * 1.02
    nLst = iterate (* 1.02) 2.0
