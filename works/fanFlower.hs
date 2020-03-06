------------------------------------------------------------
-- | Fan Flowar
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window white 100 [(st, [fanFlower])]
  where
    window = InWindow "test2" (800, 600) (10, 10)
    st = initST {mark = False, point = (125, 34)}


-- | FanFlower
--   from "http://deepakjois.github.io/hs-logo/"
fanFlower :: Command
fanFlower = repCommand 12 [fan, qf 250]
  where fan = repCommand 75 [qf 100, bk 100, rt 2]
