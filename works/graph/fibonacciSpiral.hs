------------------------------------------------------------
-- | Fibonacci Spiral
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle initDisp white 100 [(st, [grid' 500 1, fibonacciSpiral 15])]
  where st = initST {angle = -90}


-- | Fibonacci 数列
fibonacciList :: [Float]
fibonacciList = fib' 1 1 where fib' a b = seq a $ a : fib' b (a + b)

-- | Fibonacci Spiral
fibonacciSpiral :: Int -> Command
fibonacciSpiral n = concat [drawArcL 90 r | r <- take n fibonacciList]
