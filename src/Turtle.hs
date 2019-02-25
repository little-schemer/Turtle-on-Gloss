--------------------------------------------------------------------------------
-- |
--   Module      : Turtle
--   Description : Turtle Graphics on Gloss
--   Copyright   : (c) little Haskeller, 2018
--   License     : BSD3
--
--   Gloss を使った Turtle Graphics
--
--------------------------------------------------------------------------------

module Turtle where

import Graphics.Gloss


data TurtleST = TurtleST { angle    :: Float -- ^ 亀の向き
                         , point    :: Point -- ^ 亀の位置
                         , penColor :: Color -- ^ ペンの色
                         , pen      :: Bool  -- ^ up or down
                         } deriving Show

type Command = TurtleST -> (Picture, TurtleST)


--------------------------------------------------
-- * 亀の初期値の雛形
--------------------------------------------------

-- | 初期値 : angle = 0, point = (0, 0), penColor = black, pen = True
initST = TurtleST {angle = 0, point = (0, 0), penColor = black, pen = True}


--------------------------------------------------
-- * Turtle Graphics の基本コマンド
--------------------------------------------------

-- | n だけ前進する (pen == Ture なら線を描く)
forward :: Float -> Command
forward n st = (isDraw st $ Line [point st, p], st {point = p})
  where p = newPoint n (angle st) (point st)

-- | n だけ後退する (pen == Ture なら線を描く)
backward :: Float -> Command
backward n = forward (- n)

-- | th 度だけ左旋回する
left :: Float -> Command
left th st = (Blank, st {angle = h'}) where h' = angle st + th

-- | th 度だけ右旋回する
right :: Float -> Command
right th = left (- th)

-- | p の位置へ移動する (亀の向きは不変。pen == Ture なら線を描く)
goto :: Point -> Command
goto p st = (isDraw st $ Line [point st, p], st {point = p})

-- | 移動時に線を描く
penDown :: Command
penDown st = (Blank, st {pen = True})

-- | 移動時に線を描かない
penUp :: Command
penUp st = (Blank, st {pen = False})

-- | 亀の向きを設定する
setAngle :: Float -> Command
setAngle th st = (Blank, st {angle = th})

-- | 亀の位置を設定する
setPoint :: Point -> Command
setPoint p st = (Blank, st {point = p})

-- | 色を設定する
setColor :: Color -> Command
setColor c st = (Blank, st {penColor = c})


--------------------------------------------------
-- * Alias
--------------------------------------------------

-- | forward
fd = forward

-- | backward
bk = backward

-- | left
lt = left

-- | right
rt = right

-- | penUp
pu = penUp

-- | penDown
pd = penDown


--------------------------------------------------
-- * 補助関数
--------------------------------------------------

-- | 亀が n だけ前進した位置
newPoint :: Float -> Float -> Point -> Point
newPoint n th (x, y)  = (x + n * cos h', y + n * sin h')
  where h' = th * pi / 180

-- | pen の状態によって図形か Blank を返す
isDraw :: TurtleST -> Picture -> Picture
isDraw st pic = if pen st then (Color (penColor st) $ pic) else Blank


--------------------------------------------------
-- * runTurtle コマンド
--------------------------------------------------

-- |  コマンドのリストをまとめて１つのコマンドにする
runTurtle :: [Command] -> Command
runTurtle cmdLst st = foldl f (Blank, st) cmdLst
  where f (pic, st) cmd = let (pic', st') = cmd st in (pic <> pic', st')


--------------------------------------------------
-- * 図形を描くコマンド
--------------------------------------------------

-- ** 正多角形

-- | 正多角形を描く
drawPolygon :: (Float -> Command) -> Int -> Float -> Command
drawPolygon cmd n m st = (fst $ runTurtle (cs ++ [goto (point st)]) st, st)
  where
    th = 360 / (fromIntegral n)
    cs = cmd (th / 2) : (concat $ replicate (n - 1) [forward m, cmd th])

-- | 一辺の長さが m の正 n 角形を左回りに描く
drawPolygonL :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> Command
drawPolygonL = drawPolygon left

-- | 一辺の長さが m の正 n 角形を右回りに描く
drawPolygonR :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> Command
drawPolygonR = drawPolygon right


-- ** 円弧

-- | 中心角 th 半径 r の円弧を左回りに描く
drawArcL :: Float               -- ^ 中心角
         -> Float               -- ^ 半径
         -> Command
drawArcL th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
  where
    a  = angle st
    a' = a - 90
    (ox, oy) = newPoint r (a + 90) (point st)
    st' = st {angle = a + th, point = newPoint r (a + th - 90) (ox, oy)}

-- | 中心角 th 半径 r の円弧を右回りに描く
drawArcR :: Float               -- ^ 中心角
         -> Float               -- ^ 半径
         -> Command
drawArcR th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
  where
    a  = angle st
    a' = 90 + a - th
    (ox, oy) = newPoint r (a - 90) (point st)
    st' = st {angle = a - th, point = newPoint r (a - th + 90) (ox, oy)}
