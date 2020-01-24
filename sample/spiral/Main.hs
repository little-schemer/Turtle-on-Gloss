module Main where

import           Graphics.Gloss
import           Turtle


main :: IO ()
main = runTurtle window white [(initST { penColor = blue }, spiral 250)]
  where window = InWindow "Spiral" (800, 600) (10, 10)


-- | らせん
spiral :: Int -> [Command]
spiral n = take n $ concat [[fd len, rt 93] | len <- iterate (* 1.05) 2.0]
