import Graphics.Gloss
import Graphics.Turtle

main :: IO ()
main = runTurtle initWindow black 20 [(st, [flower 100])]
  where st = initST {point = (0, -250), angle = 90}


flower :: Float -> Command
flower n = concat [ setColor green
                  , fd n, push, lt 60, leaf n, pop
                  , fd n, push, rt 60, leaf n, pop
                  , fd n, pu, fd n, pd, flower' n
                  ]
  where
    flower' n = concat [setColor rose, cmd (n / 2), setColor yellow, cmd (n / 6)]
      where cmd n = repCommand 12 [lt 30, drawArcL 360 n]

    leaf n = concat [ setColor green, fd (n * 1.5), rt 135
                    , drawArcR 90 n , rt 90, drawArcR 90 n
                    ]
