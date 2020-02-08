import           Graphics.Gloss
import           Graphics.Turtle

main :: IO ()
main = runTurtle initDisp white 500 [(st, [sierpinski 8 600])]
  where st = initST {point = (-300, -250), mark = False}


-- シェルピンスキーの三角
sierpinski :: Int -> Float -> Command
sierpinski n len = if odd n
                   then concat [ql 60, sierA n len]
                   else sierA n len
  where
    sierA 0 len = qf len
    sierA n len = concat [ sierB (n - 1) (len / 2)
                         , qr 60
                         , sierA (n - 1) (len / 2)
                         , qr 60
                         , sierB (n - 1) (len / 2)
                         ]

    sierB 0 len = qf len
    sierB n len = concat [ sierA (n - 1) (len / 2)
                         , ql 60
                         , sierB (n - 1) (len / 2)
                         , ql 60
                         , sierA (n - 1) (len / 2)
                         ]
