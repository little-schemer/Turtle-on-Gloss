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


-- | Turtle Graphics のコマンドの型
type Command = TurtleST -> (TurtleST, Picture)



------------------------------------------------------------
-- * Turtle Graphics
------------------------------------------------------------

--
-- ** 亀の初期値の雛形
--

-- | 初期値 : angle = 0, point = (0, 0), penColor = black, pen = True
initST = TurtleST {angle = 0, point = (0, 0), penColor = black, pen = True}


--
-- ** Turtle Graphics の基本コマンド
--

-- | n だけ前進する (pen == Ture なら線を描く)
forward :: Float -> Command
forward n st = (st {point = p}, isDraw st $ Line [point st, p])
  where p = newPoint n st

-- | n だけ後退する (pen == Ture なら線を描く)
backward :: Float -> Command
backward n st = forward (- n) st

-- | th 度だけ左旋回する
left :: Float -> Command
left th st = (st {angle = h'}, Blank)
  where h' = angle st + th

-- | th 度だけ右旋回する
right :: Float -> Command
right th st = (st {angle = h'}, Blank)
  where h' = angle st - th

-- | p の位置へ移動する (亀の向きは不変。pen == Ture なら線を描く)
goto :: Point -> Command
goto p st = (st {point = p}, isDraw st $ Line [point st, p])

-- | 移動時に線を描く
penDown :: Command
penDown st = (st {pen = True}, Blank)

-- | 移動時に線を描かない
penUp :: Command
penUp st = (st {pen = False}, Blank)

-- | 亀の向きを設定する
setAngle :: Float -> Command
setAngle th st = (st {angle = th}, Blank)

-- | 亀の位置を設定する
setPoint :: Point -> Command
setPoint p st = (st {point = p}, Blank)

-- | 色を設定する
setColor :: Color -> Command
setColor c st = (st {penColor = c}, Blank)

-- | Alias
fd = forward
bk = backward
lt = left
rt = right
pu = penUp
pd = penDown


--
-- ** 補助関数
--

-- | 亀が n だけ前進した位置
newPoint :: Float -> TurtleST -> Point
newPoint n st = (x + n * cos h', y + n * sin h')
  where (h', (x, y)) = (angle st * pi / 180, point st)

-- | pen の状態によって図形か Blank を返す
isDraw :: TurtleST -> Picture -> Picture
isDraw st pic = if pen st then (Color (penColor st) $ pic) else Blank


--
-- ** runTurtle コマンド
--

-- |  コマンドのリストをまとめて１つのコマンドにする
runTurtle :: [Command] -> Command
runTurtle cmdLst st = foldl f (st, Blank) cmdLst
  where f (st, pic) cmd = (st', pic <> pic')
          where (st', pic') = cmd st


--
-- ** 図形を描くコマンド
--

-- | 正多角形を描く
drawPolygon :: (Float -> Command) -> Int -> Float -> Command
drawPolygon cmd n m st = (st, pic)
  where
    th = 360 / (fromIntegral n)
    cs = (concat $ replicate (n - 1) [forward m, cmd th]) ++ [goto (point st)]
    (_, pic) = runTurtle cs st

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
