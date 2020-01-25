module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window white 60 [(initST { penColor = blue }, spiral 150)]
  where window = InWindow "Spiral" (800, 600) (10, 10)


-- | らせん
spiral :: Int -> [Command]
spiral n = concat $ take n [[forward len, right 93] | len <- nLst]
  where nLst = iterate (* 1.05) 2.0
