module L_system where


import Data.Maybe (fromJust)
import Graphics.Gloss
import Turtle


--
-- L-system
--
l_system :: String               -- 初期文字列
         -> [(Char, String)]     -- 変換規則
         -> Int                  -- 繰り返し回数
         -> String               -- 文字列の最終形
l_system axiom _    0 = axiom
l_system axiom rule n = l_system (concatMap f axiom) rule (n - 1)
  where f c = if x == Nothing then [c] else fromJust x
          where x = lookup c rule


--
-- drawLine
--
--  + 文字の意味
--   'F' : 線を描きながら、亀を n だけ進める。
--   'f' : 線を描かずに、亀を n だけ進める。
--   '+' : 亀を反時計回りに th 度回転させる。
--   '-' : 亀を時計回りに th 度回転させる。
--   '[' : 亀の状態をスタックにプッシュする。
--   ']' : 亀の状態をスタックからポップする。
--   その他 : 何もしない。
--
drawLine :: (Float, Point, Color) -- ^ 亀の初期状態 (向き, 位置, 色)
         -> Float                 -- ^ 亀が 1 step で進む距離 n
         -> Float                 -- ^ 亀が 1 step で回る角度 th
         -> String                -- ^ L-system で作成された文字列
         -> Picture               -- ^ 作成された図形
drawLine (h, p, c) n th cs = Pictures $ fst $ loop (h, p, c, True) cs []
  where
    loop :: TurtleST -> String -> [Picture] -> ([Picture], String)
    loop _ [] pList = (pList, [])
    loop tST ('F' : cs) pList = loop st' cs (pic : pList)
      where (st', pic) = turtle tST (Forward n)
    loop tST ('f' : cs) pList = loop st' cs pList
      where (st', _) = turtle tST (Forward n)
    loop (h, p, c, pen) ('+' : cs) pList = loop (h + th, p, c, pen) cs pList
    loop (h, p, c, pen) ('-' : cs) pList = loop (h - th, p, c, pen) cs pList
    loop tST ('[' : cs) pList = loop tST cs' (pic ++ pList)
      where (pic, cs') = loop tST cs []
    loop tST (']' : cs) pList = (pList, cs)
    loop tST ( _  : cs) pList = loop tST cs pList
