------------------------------------------------------------
-- | Cross Stich
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | パラメータ
size  = 300
level =   4


-- | Main
main :: IO ()
main = runTurtle initDisp white 500 [(st, [lst])]
  where
    st  = initST {point = (-size / 2, size / 2), mark = False}
    lst = repCommand 4 [crossStich level size, qr 90]


-- | Cross-Stich
crossStich :: Int -> Float -> Command
crossStich 0 len = qf len
crossStich n len = concat [ crossStich (n - 1) (len / 3)
                          , ql 90
                          , crossStich (n - 1) (len / 3)
                          , qr 90
                          , crossStich (n - 1) (len / 3)
                          , qr 90
                          , crossStich (n - 1) (len / 3)
                          , ql 90
                          , crossStich (n - 1) (len / 3)
                          ]
