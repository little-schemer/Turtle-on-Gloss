------------------------------------------------------------
-- | Rotating Circle
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window white 100 [(st, [rotatingCircle])]
  where
    st = initST {point = (27, -22)}
    window = initWindow {title = "Rotating Circle"}


-- | RotatingCircle
--   from "http://deepakjois.github.io/hs-logo/"
rotatingCircle :: Command
rotatingCircle = repCommand 36 ([repCommand 34 [fd 20, rt 10], rt 90])
