------------------------------------------------------------
-- 螺旋 from https://en.wikipedia.org/wiki/Spiral
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


func :: Float -> Float
func = archimedeanSpiral 2
-- func = logarithmicSpiral 0.1 0.2


main :: IO ()
main = runTurtle initDisp white 100 [(initST, cmd)]
  where
    cmd = [grid, drawPolarGraph func lst]
    lst = [0, 0.1 .. pi * 20]


e :: Float
e = 2.718281828459045


archimedeanSpiral :: Float -> Float -> Float
archimedeanSpiral a = \th -> a * th

hyperbolicSpiral :: Float -> Float -> Float
hyperbolicSpiral a = \th -> a / th

fermat'sSpiral :: Float -> Float -> Float
fermat'sSpiral a = \th -> a * (th ** 0.5)

lituus :: Float -> Float -> Float
lituus a = \th -> a * (th ** (-0.5))

logarithmicSpiral :: Float -> Float -> Float -> Float
logarithmicSpiral a k = \th -> a * (e ** (k * th))
