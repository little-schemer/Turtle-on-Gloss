-- from "http://deepakjois.github.io/hs-logo/"

import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 100 [(initST, rotatingCircle)]
  where window = InWindow "test2" (800, 600) (10, 10)


rotatingCircle :: [Command]
rotatingCircle = repCommand 36 (repCommand 34 [fd 12, rt 10] ++ [rt 90])
