import           Graphics.Gloss
import           Graphics.Turtle


size  = 300
level =   4

main :: IO ()
main = runTurtle initDisp white 500 [(st, [lst])]
  where st = initST {point = (-size / 2, size / 2), mark = False}

lst :: Command
lst = repCommand 4 [crossStich level size, qr 90]


-- Cross-stich
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
