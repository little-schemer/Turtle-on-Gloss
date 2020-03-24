------------------------------------------------------------
-- | 星型を描く
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- Main
main :: IO ()
main = runTurtle window white 30 [(initST { point = (-100, -100) }, cmdLst)]
  where
    window = initWindow {title = "Star"}
    cmdLst = concat [[star n, lt 45, pu, fd (n * 2), pd] | n <- ls]
      where ls = [120, 60, 30, 15, 7.5]


-- | 星型を描く
star :: Float -> Command
star n = repCommand 5 [fd n, lt 72, fd n, rt 144]
