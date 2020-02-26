------------------------------------------------------------
-- |
--   Module    : Cycloid
--   Copyright : (c) little Haskeller, 2020
--   License   : BSD3
--
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle


-- | 外サイクロイドまたは内サイクロイド
func :: Float -> Point
func = epicycloid 100 85

-- | 回転角のリスト
rotationAngles :: [Float]
rotationAngles = [0, 0.1 .. 300]


main :: IO ()
main = runTurtle initDisp white 100 [(st, cmds)]
  where
    st   = initST {point = func 0, mark = False}
    cmds = [goto $ func th | th <- rotationAngles]


--
-- | 外サイクロイド epicycloid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + th : 回転角
--
--  x = (rc + rm) * cos th - rm * cos ((rc + rm) / rm * th)
--  y = (rc + rm) * sin th - rm * sin ((rc + rm) / rm * th)
--
epicycloid :: Float -> Float -> Float -> Point
epicycloid rc rm th = (f cos, f sin)
  where f func = r' * func th - rm * func (r' / rm * th)
          where r' = rc + rm

--
-- | 内サイクロイド hypocycloid
--
--  + rc : 定円の半径
--  + rm : 動円の半径
--  + th : 回転角
--
--  x = (rc - rm) * cos th + rm * cos ((rc - rm) / rm * th)
--  y = (rc - rm) * sin th + rm * sin ((rc - rm) / rm * th)
--
hypocycloid :: Float -> Float -> Float -> Point
hypocycloid rc rm th = (f cos, f sin)
  where f func = dr * func th + rm * func (dr / rm * th)
          where dr = rc - rm
