module Turtle where


import Graphics.Gloss


-- 亀の状態 : (向き, 位置, 色, Pen (up / down))
type TurtleST = (Float, Point, Color, Bool)

-- Turtle Graphics のコマンド
type Command = TurtleST -> (TurtleST, Picture)

--
-- Turtle Commands
--

-- n だけ前進する
forward :: Float -> Command
forward n (h, (x, y), c, pen) = ((h, p, c, pen), pic)
  where
    h'  = h * pi / 180
    p   = (x + n * cos h', y + n * sin h')
    pic = if pen then (Color c $ Line [(x, y), p]) else Blank

-- n だけ後退する
backward :: Float -> Command
backward n tST = forward (- n) tST

-- th 度だけ左旋回する
turnLeft :: Float -> Command
turnLeft th (h, p, c, pen) = ((h + th, p, c, pen), Blank)

-- th 度だけ右旋回する
turnRight :: Float -> Command
turnRight th tST = turnLeft (- th) tST

-- p へ移動する
goto :: Point -> Command
goto p (h, p', c, pen) = ((h, p, c, pen), pic)
  where pic = if pen then (Color c $ Line [p', p]) else Blank

-- 移動時に線を描く
penDown :: Command
penDown (h, p, c, _) = ((h, p, c, True), Blank)

-- 移動時に線を描かない
penUp :: Command
penUp (h, p, c, _) = ((h, p, c, False), Blank)

-- 線の色を設定する
setColor :: Color -> Command
setColor c (h, p, _, pen) = ((h, p, c, pen), Blank)

-- 亀の位置を設定する
setPoint :: Point -> Command
setPoint p (h, _, c, pen) = ((h, p, c, pen), Blank)

-- 亀の向きを設定する
setAngle :: Float -> Command
setAngle th (_, p, c, pen) = ((th, p, c, pen), Blank)


--
-- runTurtle
--
runTurtle :: TurtleST -> [Command] -> (TurtleST, [Picture])
runTurtle tST cmdList = foldl f (tST, []) cmdList
  where f (tST, pList) cmd = (tST', pic : pList)
          where (tST', pic) = cmd tST
