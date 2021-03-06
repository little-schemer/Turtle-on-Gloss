import           Graphics.Gloss
import           Graphics.Turtle

main :: IO ()
main = runTurtle initWindow black 30 [(st, [flower 100])]
  where st = initST {point = (0, -250), heading = 90}


flower :: Float -> Command
flower n = concat [ setColor green
                  , setThickness 10
                  , fd n, push, lt 60, leaf n, pop
                  , fd n, push, rt 60, leaf n, pop
                  , fd n, pu, fd n, pd, flower' n
                  ]
  where
    flower' n = concat [ setColor rose
                       , setThickness 10
                       , cmd1 (n / 2)
                       , setThickness 0
                       , setColor yellow, lt 30, cmd2 (n / 3)
                       , setColor orange, lt 60, cmd2 (n / 3) ]
      where
        cmd1 n = repCommand 12 [lt 30, drawArcR 360 n]
        cmd2 n = concat
                 $ replicate 3
                 $ drawPolygon [fd n, rt 120, fd n, rt 120, fd n]

    leaf n = concat [ setColor green, fd (n * 1.5), rt 135
                    , drawArcR 90 n, rt 90, drawArcR 90 n]
