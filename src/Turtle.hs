module Turtle where


import Graphics.Gloss


-- 亀の状態 : (向き, 位置, 色, Pen (up / down))
type TurtleST = (Float, Point, Color, Bool)

-- コマンド
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
left :: Float -> Command
left th (h, p, c, pen) = ((h + th, p, c, pen), Blank)

-- th 度だけ右旋回する
right :: Float -> Command
right th (h, p, c, pen) = ((h - th, p, c, pen), Blank)

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
runTurtle :: [Command] -> Command
runTurtle cmdLst tST = loop cmdLst tST []
  where
    loop [] tST picLst = (tST, Pictures picLst)
    loop (cmd : cmdLst) tST picLst = loop cmdLst tST' (pic : picLst)
      where (tST', pic) = cmd tST


-- 円を描く
tCircle :: (Float -> Command) -> Float -> Command
tCircle cmd r tST = runTurtle cmdLst tST
  where
    n = 2 * r * sin (5 * pi / 180)
    cmdLst = concat $ replicate 36 [cmd 5, forward n, cmd 5]

-- 半径 r の右回りの円を描く
circleR :: Float -> Command
circleR r tST = tCircle right r tST

-- 半径 r の左回りの円を描く
circleL :: Float -> Command
circleL r tST = tCircle left r tST

-- 亀の位置を中心に半径 r の円を描く
drawCircle :: Float -> Command
drawCircle r tST@(_, (x, y), c, _) = (tST, pic)
  where pic = Translate x y $ Color c $ Circle r
