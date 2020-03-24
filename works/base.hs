------------------------------------------------------------
-- |
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window white 20 [(st, lst)]
  where
    window = initWindow
    st     = initST
    lst    = [fd 200, rt 120, fd 200, rt 120, fd 200]
