------------------------------------------------------------
-- |
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle
import Graphics.L_system


-- | Main
main :: IO ()
main = runTurtle initWindow white 20 [(st, [lst])]
  where
    st  = initST
    lst = l_system "F" [('F', "F+F--F+F")] 2 30 60
