module Main where

import           Graphics.Gloss
import           Graphics.Turtle

main :: IO ()
main = runTurtle window white 5 [ (initST {angle =  0, penColor = red}, hex) ]
  where window = InWindow "test" (800, 600) (10, 10)

hex :: [Command]
hex = take 20 $ cycle [ setColor blue
                      , forward 100
                      , drawCircleSolid 100
                      , left 60
                      , setColor red
                      , forward 100
                      , drawCircleSolid 100
                      , left 60
                      ]
