--------------------------------------------------------------------------------
-- |
--   Module      : L_system
--   Description : L-system
--   Copyright   : (c) little Haskeller, 2018, 2019, 2020
--   License     : BSD3
--
--   L-system により Turtle Graphics のコマンドを作る。
--
--------------------------------------------------------------------------------

module Graphics.L_system where


import           Data.Char
import           Data.Maybe      (fromJust)
import           Graphics.Gloss
import           Graphics.Turtle


------------------------------------------------------------
-- * L-system
------------------------------------------------------------

--
-- | 初期文字列を変換規則に基づいて繰り返し書き換えていき、できあがった文字列を
--   Turtle Graphics のコマンドに変換する
--
-- > 《 文字の意味 》
-- >   A 〜 F : 亀を n だけ進める (線を描く)
-- >   a 〜 f : 亀を n だけ進める (線を描かない)
-- >   +      : 亀を反時計回りに th 度旋回させる。
-- >   -      : 亀を時計回りに th 度旋回させる。
-- >   [      : 亀の状態をスタックにプッシュする。
-- >   ]      : 亀の状態をスタックからポップする。
-- >   その他 : なにもしない。
--
l_system :: String              -- ^ 初期文字列
         -> [(Char, String)]    -- ^ 変換規則
         -> Int                 -- ^ 繰り返し回数
         -> Float               -- ^ 亀が 1 step で進む距離 n
         -> Float               -- ^ 亀が 1 step で旋回する角度 th
         -> Command
l_system axiom rule c n th = comvToCmd n th cs
  where
    cs = makeString axiom rule c

    -- | L-system のルールに従って文字列を作る
    makeString axiom rule n
      | n < 1     = axiom
      | otherwise = makeString (concatMap f axiom) rule (n - 1)
      where f c = if x == Nothing then [c] else fromJust x
              where x = lookup c rule

    -- | 文字列を Turtle Graphics の Command に変換する
    comvToCmd n th cs = concatMap f cs
      where
        f '+' = ql th
        f '-' = qr th
        f '[' = push
        f ']' = pop
        f '|' = ql 180
        f c
          | elem c "ABCDEF" = qf n
          | elem c "abcdef" = concat [pu, qf n, pd]
          | otherwise = []
