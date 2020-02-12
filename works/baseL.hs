import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


main :: IO ()
main = runTurtle initDisp white 20 [(st, [lst])]
  where
    st  = initST
    lst = l_system "F" [('F', "F+F--F+F")] 2 30 60
