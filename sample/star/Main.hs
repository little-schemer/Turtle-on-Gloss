module Main where

import           Graphics.Gloss
import           Graphics.Turtle


main :: IO ()
main = runTurtle window white 30 [(initST { point = (-100, -100) }, cmdLst)]
  where
    window = InWindow "Star" (800, 600) (10, 10)
    cmd len = star len ++ [lt 45, pu, fd (len * 2), pd]
    cmdLst  = concatMap cmd [120, 60, 30, 15, 7.5]

star :: Float -> [Command]
star len = repCommand 5 [fd len, lt 72, fd len, rt 144]
