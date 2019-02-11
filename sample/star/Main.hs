module Main where

import Graphics.Gloss
import Turtle

main :: IO ()
main = display window white pic
  where
    window  = InWindow "Spiral" (800, 600) (10, 10)
    cmd len = [star len, lt 45, pu, fd (len * 2), pd]
    cmdLst  = concatMap cmd [120, 60, 30, 15, 7.5]
    pic     = snd $ runTurtle cmdLst initST {point = (-100, -100)}


-- | 星型
star :: Float -> Command
star len st = (st, snd $ runTurtle cmdLst st)
  where cmdLst = take 20 $ cycle [fd len, lt 72, fd len, rt 144]
