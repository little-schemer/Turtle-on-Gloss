import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window (greyN 0.3) 20 tLst
  where
    window = InWindow "Circle" (800, 600) (10, 10)
    col   = [setColor c | c <- cycle [red, green, blue]]
    angle = [setAngle th | th <- [30, 60 .. 360]]
    tLst  = [(initST, [a, b, drawArcL 360 100]) | (a, b) <- zip col angle]
