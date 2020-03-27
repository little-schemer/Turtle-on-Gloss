------------------------------------------------------------
-- | Sierpinski の三角形 (再帰版)
------------------------------------------------------------

import Graphics.Gloss
import Graphics.Turtle


len   = 300 :: Float
level = 8   :: Int


-- | Main
main :: IO ()
main = runTurtle window white 100 [(st, lst)]
  where
    window = initWindow
    st     = initST {point = (-len, -len / 1.2), mark = False}
    lst    = [sierpinski level len]


-- | 再帰による Sierpinski の三角形
sierpinski :: Int -> Float -> Command
sierpinski n size = concat [ cmd n size, ql 120, qf (size * 2), ql 120, qf (size * 2)]
  where
    cmd 1 size = concat [ qf (size * 2) ]
    cmd n size = concat [ cmd (n - 1) (size / 2), ql 120, qf size, qr 120
                        , cmd (n - 1) (size / 2), qr 120, qf size, ql 120
                        , cmd (n - 1) (size / 2)
                        ]
