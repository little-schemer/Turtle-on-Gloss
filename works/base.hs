------------------------------------------------------------
-- |
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 20 [(st, lst)]
  where
    st  = initST
    lst = [fd 200, rt 120, fd 200, rt 120, fd 200]
