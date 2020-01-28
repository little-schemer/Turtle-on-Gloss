import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 100 [(st1, kh), (st2, kh), (st3, kh)]
  where
    window = InWindow "Koch Curve" (800, 600) (10, 10)
    kh = [kochCurve 400 4]
    st1 = initST { angle    = 0
                 , point    = (-200, 200 / sqrt 3)
                 , mark     = False
                 }

    st2 = initST { angle    = -120
                 , point    = (200, 200 / sqrt 3)
                 , mark     = False
                 }

    st3 = initST { angle    = 120
                 , point    = (0, -400 / sqrt 3)
                 , mark     = False
                 }


-- | 再帰関数によるコッホ曲線
kochCurve :: Float -> Int -> Command
kochCurve len n = kh len n
  where
    kh len 0 = qf len
    kh len n = concat [kh', ql 60, kh', qr 120, kh', ql 60, kh']
      where kh' = kh (len / 3) (n - 1)
