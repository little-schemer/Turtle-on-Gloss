-- --------------------------------------------------------------------------------
-- -- |
-- --   Module      : L_system
-- --   Description : L-system
-- --   Copyright   : (c) little Haskeller, 2018
-- --   License     : BSD3
-- --
-- --   L-system
-- --------------------------------------------------------------------------------

module L_system where

import           Data.Maybe      (fromJust)
import           Graphics.Gloss
import           Graphics.Turtle


------------------------------------------------------------
-- * L-system
------------------------------------------------------------

--
-- | 与えられた文字列を変換規則に基づいて書き換えていく。
--
-- >>> l_system "F" [('F', "XF"), ('X', "FF")] 3
-- "XFXFFFXF"
--
l_system :: String               -- ^ 初期文字列
         -> [(Char, String)]     -- ^ 変換規則
         -> Int                  -- ^ 繰り返し回数
         -> String               -- ^ 文字列の最終形
l_system axiom _    0 = axiom
l_system axiom rule n = l_system (concatMap f axiom) rule (n - 1)
  where f c = if x == Nothing then [c] else fromJust x
          where  x = lookup c rule


------------------------------------------------------------
-- * drawLine
------------------------------------------------------------

--
-- | 与えられた文字列に従って、Turtle Graphics で描いた画像を作る
--
-- > * 文字の意味
-- >   'F' : 線を描きながら、亀を n だけ進める。
-- >   'f' : 線を描かずに、亀を n だけ進める。
-- >   '+' : 亀を反時計回りに th 度回転させる。
-- >   '-' : 亀を時計回りに th 度回転させる。
-- >   '[' : 亀の状態をスタックにプッシュする。
-- >   ']' : 亀の状態をスタックからポップする。
-- >   その他 : 何もしない。
drawLine :: TurtleST            -- ^ 亀とペンの初期状態
         -> Float               -- ^ 亀が 1 step で進む距離 n
         -> Float               -- ^ 亀が 1 step で回る角度 th
         -> String              -- ^ L-system で作成された文字列
         -> [Picture]           -- ^ 作成された図形
drawLine st n th cs = pic
  where
    (_, pic, _) = foldl f (st, [Blank], []) cs
    f (st, pic, stack) c = case c of
      'F' -> (st', pic ++ pic', stack) where (pic', st') = fd n st
      'f' -> (st {point = newPoint n (angle st) (point st)}, pic, stack)
      '+' -> (st {angle = angle st + th}, pic, stack)
      '-' -> (st {angle = angle st - th}, pic, stack)
      '[' -> (st, pic, (st : stack))
      ']' -> (head stack, pic, tail stack)
      _   -> (st, pic, stack)
