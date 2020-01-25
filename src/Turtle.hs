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


import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort


data TurtleST = TurtleST { angle    :: Float -- ^ 亀の向き
                         , point    :: Point -- ^ 亀の位置
                         , penColor :: Color -- ^ ペンの色
                         , pen      :: Bool  -- ^ up or down
                         , mark     :: Bool  -- ^ 亀のマーク
                         } deriving Show

type PrimitiveCommand  = TurtleST -> (Picture, TurtleST)

type Command = [PrimitiveCommand]

type TurtleData = (TurtleST, [Command])

type Model = (Picture, [TurtleST], [(TurtleST, Command)])


--------------------------------------------------
-- * 亀の初期値の雛形
--------------------------------------------------

initST = TurtleST { angle = 0
                  , point = (0, 0)
                  , penColor = black
                  , pen = True
                  , mark = True }


---------------------------------------------------
-- * runTurtle
---------------------------------------------------

runTurtle :: Display -> Color -> Int -> [TurtleData] -> IO ()
runTurtle disp c step tds = simulate disp c step (Blank, [], map f tds) draw sim
  where f (st, lst) = (st, concat lst)

draw :: Model -> Picture
draw (pic, ss, _) = pic <> (Pictures $ map f ss)
  where
    tMark = Polygon [(0, -3), (0, 3), (8, 0)]
    f st = Translate x y $ Rotate (360 - angle st) $ Color (penColor st) $ tMark
      where (x, y) = point st

sim :: ViewPort -> Float -> Model -> Model
sim _ _ (pic, _, [])  = (pic, [], [])
sim _ _ (pic, _, lst) = foldl f (pic, [], []) lst
  where
    f tData (_, [])                  = tData
    f (pic, ss, tLst) (st, (c : cs)) = (pic <> pic', st' : ss, (st', cs) : tLst)
      where (pic', st') = c st


---------------------------------------------------
-- * 補助関数
---------------------------------------------------

newPoint :: Float -> TurtleST -> Point
newPoint n st = (x + n * cos th', y + n * sin th')
  where
    (x, y) = point st
    th'    = (angle st) * pi / 180

toPoint :: Point -> PrimitiveCommand
toPoint p st = (isDraw st $ Line [point st, p], st { point = p })

move :: Float -> PrimitiveCommand
move n st = toPoint (newPoint n st) st

turn :: Float -> PrimitiveCommand
turn th st = (Blank, st { angle = th' }) where th' = angle st + th

isDraw :: TurtleST -> Picture -> Picture
isDraw st pic = if (pen st) then (Color (penColor st) $ pic) else Blank


---------------------------------------------------
-- * 基本コマンド
---------------------------------------------------

-- | n だけ前進する (pen == True なら線を描く)
forward :: Float -> Command
forward n
  | n <= 50   = [move n]
  | otherwise = move 50 : forward (n - 50)

-- | n だけ後退する (pen == True なら線を描く)
backward :: Float -> Command
backward n
  | n <= 50   = [move (- n)]
  | otherwise = move (-50) : backward (n - 50)

-- | th 度だけ左旋回する
left :: Float -> Command
left th
  | th <= 30  = [turn th]
  | otherwise = turn 30 : left (th - 30)

-- | th 度だけ右旋回する
right :: Float -> Command
right th
  | th <= 30  = [turn (-th)]
  | otherwise = turn (-30) : right (th - 30)

-- | p の位置へ移動する（亀の向きは不変。 pen == True なら線を描く）
goto :: Point -> Command
goto p = [toPoint p]

-- | 亀の向きを設定する
setAngle :: Float -> Command
setAngle th = [\st -> (Blank, st {angle = th})]

-- | 亀の位置を設定する
setPoint :: Point -> Command
setPoint p = [\st -> (Blank, st {point = p})]

-- | 色を設定する
setColor :: Color -> Command
setColor c = [\st -> (Blank, st {penColor = c})]

-- | 移動時に線を描く
penDown :: Command
penDown = [\st -> (Blank, st {pen = True})]

-- | 移動時に線を描かない
penUp :: Command
penUp = [\st -> (Blank, st {pen = False})]


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
-- * 図形を描くコマンド
--------------------------------------------------

-- ** 正多角形

-- | 正多角形を描く
drawPolygon :: (Float -> Command) -> Int -> Float -> [Command]
drawPolygon cmd n m = cs ++ [cmd (- th / 2)]
  where
    th = 360 / (fromIntegral n)
    cs = cmd (th / 2) : (concat $ replicate n [forward m, cmd th])

-- | 一辺の長さが m の正 n 角形を左回りに描く
drawPolygonL :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> [Command]
drawPolygonL = drawPolygon left

-- | 一辺の長さが m の正 n 角形を右回りに描く
drawPolygonR :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> [Command]
drawPolygonR = drawPolygon right


-- ** 円

-- | 亀の位置を中心に、半径 r の円を描く
drawCircle :: Float -> Command
drawCircle r = [drawCircle' r]
  where drawCircle' r st = (isDraw st $ Translate x y $ Circle r, st)
          where (x, y) = point st


-- ** 円弧

-- -- | 中心角 th 半径 r の円弧を左回りに描く
-- drawArcL :: Float               -- ^ 中心角
--          -> Float               -- ^ 半径
--          -> PrimitiveCommand
-- drawArcL th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
--   where
--     a  = angle st
--     a' = a - 90
--     (ox, oy) = newPoint r (a + 90) (point st)
--     st' = st {angle = a + th, point = newPoint r (a + th - 90) (ox, oy)}

-- -- | 中心角 th 半径 r の円弧を右回りに描く
-- drawArcR :: Float               -- ^ 中心角
--          -> Float               -- ^ 半径
--          -> PrimitiveCommand
-- drawArcR th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
--   where
--     a  = angle st
--     a' = 90 + a - th
--     (ox, oy) = newPoint r (a - 90) (point st)
--     st' = st {angle = a - th, point = newPoint r (a - th + 90) (ox, oy)}
