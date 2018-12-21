module Turtle where


import Graphics.Gloss


-- | 亀の状態 : (向き, 位置, 色, Pen (up / down))
type TurtleST = (Float, Point, Color, Bool)


--
-- Turtle Commands
--

-- | n だけ前進する
forward :: Float -> TurtleST -> (TurtleST, Picture)
forward n (h, (x, y), c, pen) = ((h, p, c, pen), pic)
  where
    h'  = h * pi / 180
    p   = (x + n * cos h', y + n * sin h')
    pic = if pen then (Color c $ Line [(x, y), p]) else Blank

-- | n だけ後退する
backward :: Float -> TurtleST -> (TurtleST, Picture)
backward n tST = forward (- n) tST

-- | th 度だけ左旋回する
turnLeft :: Float -> TurtleST -> (TurtleST, Picture)
turnLeft th (h, p, c, pen) = ((h + th, p, c, pen), Blank)

-- | th 度だけ右旋回する
turnRight :: Float -> TurtleST -> (TurtleST, Picture)
turnRight th tST = turnLeft (- th) tST

-- | p へ移動する
goto :: Point -> TurtleST -> (TurtleST, Picture)
goto p (h, p', c, pen) = ((h, p, c, pen), pic)
  where pic = if pen then (Color c $ Line [p', p]) else Blank

-- | 線を描く
penDown :: TurtleST -> (TurtleST, Picture)
penDown (h, p, c, _) = ((h, p, c, True), Blank)

-- | 線を描かない
penUp :: TurtleST -> (TurtleST, Picture)
penUp (h, p, c, _) = ((h, p, c, False), Blank)

-- | 色を設定する
setColor :: Color -> TurtleST -> (TurtleST, Picture)
setColor c (h, p, _, pen) = ((h, p, c, pen), Blank)

-- | 位置を設定する
setPoint :: Point -> TurtleST -> (TurtleST, Picture)
setPoint p (h, _, c, pen) = ((h, p, c, pen), Blank)

-- | 亀の向きを設定する
setAngle :: Float -> TurtleST -> (TurtleST, Picture)
setAngle th (_, p, c, pen) = ((th, p, c, pen), Blank)


--
-- runTurtle
--
--   - [command] を受けとって、(TurtleST, [Picture]) を返す
--
runTurtle :: TurtleST -> [TurtleST -> (TurtleST, Picture)] -> (TurtleST, [Picture])
runTurtle tST cmdList = foldl f (tST, []) cmdList
  where
    f (tST, pList) cmd = let (tST', pic) = cmd tST
                         in (tST', pic : pList)
