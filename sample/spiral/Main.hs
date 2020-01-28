import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 60 [(st, spiral 300)]
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    st = initST {point = (-1, 1), penColor = blue, mark = False}


-- | らせん
spiral :: Int -> [Command]
spiral n = concat $ take n [[fd len, qr 92] | len <- nLst]
  where nLst = iterate (* 1.02) 2.0
