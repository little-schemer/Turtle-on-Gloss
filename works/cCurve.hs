import           Graphics.Gloss
import           Graphics.Turtle


size  = 200
level = 10

main :: IO ()
main = runTurtle initDisp white 200 [(st, [cCurve level size])]
  where st = initST {point = ((-size) / 2, size / 4), mark = False}


-- C curve
cCurve :: Int -> Float -> Command
cCurve 0 len = qf len
cCurve n len = concat [qr 45, cCurve (n - 1) len', ql 90, cCurve (n - 1) len', qr 45]
  where len' = len * sqrt 2 / 2
