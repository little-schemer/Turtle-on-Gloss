------------------------------------------------------------
-- | Rotating Circle
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | RotatingCircle
--   from "http://deepakjois.github.io/hs-logo/"
rotatingCircle :: Command
rotatingCircle = repCommand 36 ([repCommand 34 [fd 12, rt 10], rt 90])


-- | Main
main :: IO ()
main = runTurtle window white 100 [(initST {point = (16, -13)}, [rotatingCircle])]
  where window = InWindow "Rotating Circle" (800, 600) (10, 10)
