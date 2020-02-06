-- --------------------------------------------------------------------------------
-- -- |
-- --   Module      : L_system
-- --   Description : L-system
-- --   Copyright   : (c) little Haskeller, 2018
-- --   License     : BSD3
-- --
-- --   L-system
-- --------------------------------------------------------------------------------

module Graphics.L_system where

import           Data.Char
import           Data.Maybe      (fromJust)
import           Graphics.Gloss
import           Graphics.Turtle


------------------------------------------------------------
-- * L-system
------------------------------------------------------------

--
-- | L-system
--
l_system :: String              -- ^ 初期文字列
         -> [(Char, String)]    -- ^ 変換規則
         -> Int                 -- ^ 繰り返し回数
         -> Float               -- ^ 亀が 1 step で進む距離 n
         -> Float               -- ^ 亀が 1 step で旋回する角度 th
         -> [Command]
l_system axiom rule c n th = comvToCmd n th cs
  where cs = makeString axiom rule c

--
-- | 与えられた文字列を変換規則に基づいて書き換えていく。
--
-- >>> makeString "F" [('F', "XF"), ('X', "FF")] 3
-- "XFXFFFXF"
--
makeString :: String               -- ^ 初期文字列
           -> [(Char, String)]     -- ^ 変換規則
           -> Int                  -- ^ 繰り返し回数
           -> String               -- ^ 文字列の最終形
makeString axiom _    0 = axiom
makeString axiom rule n = makeString (concatMap f axiom) rule (n - 1)
  where f c = let x = lookup c rule in if x == Nothing then [c] else fromJust x

--
-- | 与えられた文字列を Turtle Graphics の Command に変換していく。
--
-- > * 文字列の意味
-- >   A 〜 F : 亀を n だけ進める (線を描く)
-- >   a 〜 f : 亀を n だけ進める (線を描かない)
-- >   '+'    : 亀を反時計回りに th 度旋回させる。
-- >   '-'    : 亀を時計回りに th 度旋回させる。
-- >   '['    : 亀の状態をスタックにプッシュする。
-- >   ']'    : 亀の状態をスタックからポップする。
-- >   その他 : なにもしない。
--
comvToCmd :: Float              -- ^ 亀が 1 step で進む距離 n
          -> Float              -- ^ 亀が 1 step で旋回する角度 th
          -> String             -- ^ makeString で作成された文字列
          -> [Command]
comvToCmd n th cs = map f cs
  where
    f '+' = ql th
    f '-' = qr th
    f '[' = push
    f ']' = pop
    f '|' = ql 180
    f c
      | elem c "ABCDEF" = qf n
      | elem c "abcdef" = concat [pu, qf n, pd]
      | otherwise = nop
