-- from "http://deepakjois.github.io/hs-logo/"

import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 100 [(initST {mark = False}, fanFlower)]
  where window = InWindow "test2" (800, 600) (10, 10)


fanFlower :: [Command]
fanFlower = repCommand 12 (repCommand 75 [qf 100, bk 100, rt 2] ++ [qf 250])
