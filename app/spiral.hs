import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
  where
    window = InWindow "Spiral" (800, 600) (10, 10)
    pic    = snd $ runTurtle cmds initST
    cmds   = take 250 $ concat [[forward n, right 93] | n <- iterate (* 1.05) 2]
