module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
  where
    window  = InWindow "Spiral" (800, 600) (10, 10)
    cmd len = [star len, penUp, left 30, forward len, penDown]
    cmdLst  = cmd 200 ++ cmd 100 ++ cmd 50 ++ cmd 25
    pic     = snd $ runTurtle cmdLst initST {point = (-100, -100)}


-- | 星型
star :: Float -> Command
star len st = (st, snd $ runTurtle cmdLst st)
  where cmdLst = concat $ replicate 5 [forward len, right 144]
