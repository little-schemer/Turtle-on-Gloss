module Turtle where


import Graphics.Gloss


-- 亀の状態 : (向き, 位置, 色, Pen (up / down))
type TurtleST = (Float, Point, Color, Bool)

-- Turtle Graphics のコマンド
data Command = Forward   Float  -- ^ 前進する
             | Backward  Float  -- ^ 後退する
             | TurnRight Float  -- ^ 右回転する
             | TurnLeft  Float  -- ^ 左回転する
             | Goto      Point  -- ^ 直接移動する
             | PenDown          -- ^ 移動時に線を描く
             | PenUp            -- ^ 移動時に線を描かない
             | SetColor  Color  -- ^ 線の色を設定する
             | SetPoint  Point  -- ^ 亀の位置を設定する
             | SetAngle  Float  -- ^ 亀の向きを設定する


--
-- Turtle Graphics
--
turtle :: TurtleST -> Command -> (TurtleST, Picture)
turtle (h, (x, y), c, pen) (Forward n) = ((h, p, c, pen), pic)
  where
    h'  = h * pi / 180
    p   = (x + n * cos h', y + n * sin h')
    pic = if pen then (Color c $ Line [(x, y), p]) else Blank
turtle st (Backward n) = turtle st (Forward (- n))
turtle (h, p, c, pen) (TurnRight th) = ((h - th, p, c, pen), Blank)
turtle (h, p, c, pen) (TurnLeft  th) = ((h + th, p, c, pen), Blank)
turtle (h, p, c, pen) (Goto p') = ((h, p', c, pen), pic)
  where pic = if pen then (Color c $ Line [p, p']) else Blank
turtle (h, p, c, _) PenDown = ((h, p, c, True), Blank)
turtle (h, p, c, _) PenUp   = ((h, p, c, False), Blank)
turtle (h, p, _, pen) (SetColor c) = ((h, p, c, pen), Blank)
turtle (h, _, c, pen) (SetPoint p) = ((h, p, c, pen), Blank)
turtle (_, p, c, pen) (SetAngle h) = ((h, p, c, pen), Blank)


--
-- runTurtle
--
runTurtle :: TurtleST -> [Command] -> (TurtleST, [Picture])
runTurtle tST cmdList = foldl f (tST, []) cmdList
  where f (tST, pList) cmd = (tST', pic : pList)
          where (tST', pic) = turtle tST cmd
