------------------------------------------------------------
-- |
--   Module    : Star
--   Copyright : (c) little Haskeller, 2020
--   License   : BSD3
--
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | 星型を描く
star :: Float -> Command
star n = repCommand 5 [fd n, lt 72, fd n, rt 144]


main :: IO ()
main = runTurtle window white 30 [(initST { point = (-100, -100) }, cmdLst)]
  where
    window = InWindow "Star" (800, 600) (10, 10)
    cmdLst = concat [[star n, lt 45, pu, fd (n * 2), pd] | n <- ls]
      where ls = [120, 60, 30, 15, 7.5]
