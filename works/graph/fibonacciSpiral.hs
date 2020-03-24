------------------------------------------------------------
-- | Fibonacci Spiral
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


-- | Main
main :: IO ()
main = runTurtle window white 100 [(st, [grid' 100 1, fibonacciSpiral 12])]
  where
    window = initWindow {title = "Fibonacci Spiral", zoom = 10}
    st = initST {angle = -90}


-- | Fibonacci 数列
fibonacciList :: [Float]
fibonacciList = fib' 1 1 where fib' a b = seq a $ a : fib' b (a + b)

-- | Fibonacci Spiral
fibonacciSpiral :: Int -> Command
fibonacciSpiral n = concat [drawArcL 90 r | r <- take n fibonacciList]
