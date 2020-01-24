module Main where

import           Graphics.Gloss
import           Turtle

main :: IO ()
main = runTurtle window white [ (initST {angle =  0}, drawPolygonR 6 100) ]
  where window = InWindow "test" (800, 600) (10, 10)



hex :: [Command]
hex = take 11 $ cycle [fd 100, rt 60]
