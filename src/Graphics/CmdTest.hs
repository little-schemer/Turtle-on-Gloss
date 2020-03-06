module Graphics.CmdTest where


import Graphics.Gloss
import Graphics.Turtle

cmdTest :: TurtleST -> Command -> TurtleST
cmdTest st cmd = foldl (\st pc -> snd $ pc st) st cmd

