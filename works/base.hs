import           Graphics.Gloss
import           Graphics.Turtle

main :: IO ()
main = runTurtle initDisp white 20 [lst]

lst :: (TurtleST, [Command])
lst = (initST, [fd 200, rt 120, fd 200, rt 120, fd 200])
