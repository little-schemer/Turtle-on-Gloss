--------------------------------------------------------------------------------
-- |
--   Module      : Turtle
--   Description : Turtle Graphics on Gloss
--   Copyright   : (c) little Haskeller, 2018, 2019, 2020
--   License     : BSD3
--
--   Gloss を使った Turtle Graphics
--
--------------------------------------------------------------------------------

module Graphics.Turtle where


import           Data.List
import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle

------------------------------------------------------------
-- * 型
------------------------------------------------------------

--
-- | 亀の状態
--
data TurtleST = Non | TurtleST
                      { heading   :: Float -- ^ 亀の向き
                      , point     :: Point -- ^ 亀の位置
                      , thickness :: Float -- ^ 線の太さ
                      , penColor  :: Color -- ^ ペンの色
                      , pen       :: Bool  -- ^ up or down
                      , mark      :: Bool  -- ^ 亀のマーク
                      , stack     :: TurtleST
                      } deriving Show

--
-- | 画面の設定
--
data WinConfig = WinConfig
                 { title   :: String         -- ^ Window のタイトル
                 , winSize :: (Int, Int)     -- ^ Window のサイズ
                 , winPos  :: (Int, Int)     -- ^ Window の位置
                 , zoom    :: Float          -- ^ 画像の拡大率
                 , shiftXY :: (Float, Float) -- ^ 画像の移動量
                 } deriving Show

--
-- | その他
--
type PrimitiveCommand  = TurtleST -> (Picture, TurtleST)
type Command           = [PrimitiveCommand]
type Model             = (Picture, [(TurtleST, Command)])


------------------------------------------------------------
-- * 初期値の雛形
------------------------------------------------------------

--
-- | TurtleST の初期値を設定する
--
initST :: TurtleST
initST = TurtleST 0 (0, 0) 0 black True True Non

--
-- | WinConfig の初期値を設定する
--
initWindow :: WinConfig
initWindow = WinConfig "Turtle Graphics" (800, 600) (10, 10) 1 (0, 0)


------------------------------------------------------------
-- * 画像表示
------------------------------------------------------------

--
-- | 亀に図形を描かせる
--
runTurtle :: WinConfig               -- ^ 画面の状態
          -> Color                   -- ^ 背景色
          -> Int                     -- ^ 1 秒あたりのステップ数
          -> [(TurtleST, [Command])]
          -> IO ()
runTurtle window bc step tds = simulate disp bc step model drawModel simModel
  where
    disp  = InWindow (title window) (winSize window) (winPos window)
    model = (Blank, [(st, concat lst) | (st, lst) <- tds])

    -- モデルを描画する
    drawModel (pic, ts) = (Scale z z $ Translate sx sy pic) <> turtleMarks
      where
        z           = zoom window
        (sx, sy)    = shiftXY window
        turtleMarks = Pictures $ map (dispMark . fst) ts
        dispMark st = if mark st
                      then Translate x' y' $ Rotate (- heading st) $ tMark
                      else Blank
          where
            (x, y)   = point st
            (x', y') = ((x + sx) * z, (y + sy) * z)
            tMark    = (Color white mark1) <> (Color (penColor st) mark2)
              where
                mark1 = Polygon [(0, -5), (0, 5), (12, 0)]
                mark2 = Polygon [(1, -4), (1, 4), (10, 0)]

    -- モデルを変化させる
    simModel _ _ (pic, []) = (pic, [])
    simModel _ _ (pic, ts) = foldl f (pic, []) ts
      where
        f model     (_, [])        = model
        f (pic, ts) (st, cmd : cs) = (pic <> pic', (st', cs) : ts)
          where (pic', st') = cmd st

--
-- | 最終結果だけを表示する
--
dispPicture :: WinConfig               -- ^ 画面の状態
            -> Color                   -- ^ 背景色
            -> [(TurtleST, [Command])]
            -> IO ()
dispPicture window c tds = display disp c $ Scale z z $ Translate sx sy pic
  where
    disp     = InWindow (title window) (winSize window) (winPos window)
    z        = zoom window
    (sx, sy) = shiftXY window
    pic      = Pictures $ map makePicture tds
    makePicture (st, cmds) = fst $ foldl f (Blank, st) (concat cmds)
      where f (pic, st) cmd = let (pic', st') = cmd st in (pic <> pic', st')


------------------------------------------------------------
-- * 補助関数
------------------------------------------------------------

-- pen が down していれば図形を描く
isDraw :: TurtleST -> Picture -> Picture
isDraw st pic = if pen st then Color (penColor st) $ pic else Blank

-- n だけ移動した先のポイントを求める
newPoint :: Float -> Float -> Point -> Point
newPoint n th p = p PA.+ n PA.* (unitVectorAtAngle $ degToRad th)

-- 太さのある線を描く
thickLine :: Float -> Float -> Float -> Point -> Point -> Picture
thickLine n a t p1 p2 = tLine <> edge p1 p2 t
  where
    (x, y) = 0.5 PA.* (p1 PA.+ p2)
    tLine   = Translate x  y  $ Rotate (-a) $ rectangleSolid n t

-- 太さのある線の両端の処理
edge :: Point -> Point -> Float -> Picture
edge (x1, y1) (x2, y2) t = circle1 <> circle2
  where
    circle1 = Translate x1 y1 $ ThickCircle (t / 4) (t / 2)
    circle2 = Translate x2 y2 $ ThickCircle (t / 4) (t / 2)

-- 角度を 0 <= th < 360 に正規化する
--   - Graphics.Gloss.Geometry.Angle の normalizeAngle を流用
normalize :: Float -> Float
normalize th = th - 360 * (fromIntegral $ floor $ th / 360)

-- Point p0 を中心に、 Point p を th 度回転させた Point p'
rotate' :: Point -> Point -> Float -> Point

rotate' p1 p0 th = rotateV (degToRad th) (p1 PA.- p0) PA.+ p0

-- 極座標 -> 直交座標
polarToRectangular :: (Float, Float) -> (Float, Float)
polarToRectangular (r, th) = (r * cos th, r * sin th)


------------------------------------------------------------
-- * PrimitiveCommand
------------------------------------------------------------

--
-- | n だけ前進する (pen == True なら線を描く)
--
forward' :: Float -> PrimitiveCommand
forward' n st = (isDraw st pic, st { point = p2 })
  where
    p1  = point st
    p2  = newPoint n (heading st) p1
    t   = thickness st
    pic = if t == 0 then Line [p1, p2] else thickLine n (heading st) t p1 p2

--
-- | Point p2 の位置へ移動する (pen == True なら線を描く, 亀は進行方向へ向く)
--
goto' :: Point -> PrimitiveCommand
goto' p2 st = (isDraw st pic, st { heading = a, point = p2 })
  where
    p1  = point st
    a   = radToDeg $ argV $ p2 PA.- p1
    t   = thickness st
    pic = if t == 0 then Line [p1, p2] else thickLine n a t p1 p2
      where n = magV (p2 PA.- p1)

--
-- | th 度だけ旋回する (th > 0 : 左旋回, th < 0 : 右旋回)
--
turn :: Float -> PrimitiveCommand
turn th st = (Blank, st { heading = normalize $ th + heading st })

--
-- | 亀の位置を中心に円を描く
--
drawCircle' :: Float -> Float -> PrimitiveCommand
drawCircle' r t st = (isDraw st $ Translate x y $ ThickCircle r t, st)
  where (x, y) = point st

--
-- | 円弧を描く
--
drawArc' :: Float -> Float -> PrimitiveCommand
drawArc' th r st = (isDraw st pic', st { heading = a', point = p' })
  where
    (a, p, t) = (heading st, point st, thickness st)
    (xo, yo) = newPoint r (if th > 0 then a + 90 else a - 90) p
    (a', p') = (a + th, rotate' p (xo, yo) th)
    pic = Translate xo yo $ Rotate rol $ ThickArc 0 (abs th) r t
      where rol = if th > 0 then 90 - a else 270 - a'
    pic' = if t > 0 then pic <> edge p p' t else pic


------------------------------------------------------------
-- * 基本コマンド
------------------------------------------------------------

--
-- | 前進する (pen == True なら線を描く)
--
forward :: Float                -- ^ 前進する距離
        -> Command
forward n
  | n <= 50   = [forward' n]
  | otherwise = forward' 50 : forward (n - 50)

--
-- | 高速に前進する
--
quickForward :: Float           -- ^ 前進する距離
             -> Command
quickForward n = [forward' n]

--
-- | 後退する (pen == True なら線を描く)
--
backward :: Float               -- ^ 後退する距離
         -> Command
backward n
  | n <= 50   = [forward' (-n)]
  | otherwise = forward' (-50) : backward (n - 50)

--
-- | 左旋回する
--
left :: Float                   -- ^ 旋回する角度 (degree)
     -> Command
left th
  | th <= 30  = [turn th]
  | otherwise = turn 30 : left (th - 30)

--
-- | 高速に左旋回する
--
quickLeft :: Float              -- ^ 旋回する角度 (degree)
          -> Command
quickLeft th = [turn th]

--
-- | 右旋回する
--
right :: Float                  -- ^ 旋回する角度 (degree)
      -> Command
right th
  | th <= 30  = [turn (-th)]
  | otherwise = turn (-30) : right (th - 30)

--
-- | 高速に右旋回する
--
quickRight :: Float             -- ^ 旋回する角度 (degree)
           -> Command
quickRight th = [turn (-th)]

--
-- | Point (x, y) の位置へ移動する (pen == True なら線を描く)
--
goto :: Point                   -- ^ 移動先の Point
     -> Command
goto p = [goto' p]

--
-- | 亀の向きを設定する
--
setHeading :: Float               -- ^ 新しい亀の向き (degree)
         -> Command
setHeading th = [\st -> (Blank, st { heading = normalize th })]

--
-- | 亀の位置を設定する
--
setPoint :: Point               -- ^ 新しい亀の位置
         -> Command
setPoint p = [\st -> (Blank, st { point = p })]

--
-- | ペンの太さを設定する
--
setThickness :: Float           -- ^ 線の太さ
             -> Command
setThickness t = [\st -> (Blank, st { thickness = t })]

--
-- | ペンの色を設定する
--
setColor :: Color               -- ^ 線の色
         -> Command
setColor col = [\st -> (Blank, st { penColor = col })]

--
-- | 移動時に線を描ように設定する
--
penDown :: Command
penDown = [\st -> (Blank, st { pen = True })]

--
-- | 移動時に線を描かないように設定する
--
penUp :: Command
penUp = [\st -> (Blank, st { pen = False })]

--
-- | 亀の状態を Push する
--
push :: Command
push = [\st -> (Blank, st { stack = st })]

--
-- | 亀の状態を Pop する
--
pop :: Command
pop = [\st -> (Blank, stack st)]

--
-- | 何もしない
--
nop :: Command
nop = [\st -> (Blank, st)]

--
-- | nop を n 回繰り返す
--
nopN :: Int                     -- ^ nop を繰り返す回数
     -> Command
nopN n = concat $ replicate n nop

--
-- | Picture を表示する
--
drawPicture :: Picture -> Command
drawPicture pic = [\st -> (pic, st)]


------------------------------------------------------------
-- * Alias
------------------------------------------------------------

-- | forward
fd = forward

-- | quickForward
qf = quickForward

-- | backward
bk = backward

-- | left
lt = left

-- | quickLeft
ql = quickLeft

-- | right
rt = right

-- | quickRight
qr = quickRight

-- | penUp
pu = penUp

-- | penDown
pd = penDown


------------------------------------------------------------
-- * 図形を描くコマンド
------------------------------------------------------------

--
-- | 亀の位置を中心に円を描く
--
drawCircle :: Float             -- ^ 半径
           -> Command
drawCircle r = [\st -> drawCircle' r (thickness st) st]

--
-- | 亀の位置を中心に solid な円を描く
--
drawCircleSolid :: Float        -- ^ 半径
                -> Command
drawCircleSolid r = [drawCircle' (r / 2) r]

--
-- | 左回りに円弧を描く
--
drawArcL :: Float                 -- ^ 中心角
       -> Float                 -- ^ 半径
       -> Command
drawArcL th r
  | th <= 30  = [drawArc' th r]
  | otherwise = drawArc' 30 r : drawArcL (th - 30) r

--
-- | 高速に左回りに円弧を描く
--
quickDrawArcL :: Float
            -> Float
            -> Command
quickDrawArcL th r = [drawArc' th r]

--
-- | 右回りに円弧を描く
--
drawArcR :: Float                 -- ^ 中心角
       -> Float                 -- ^ 半径
       -> Command
drawArcR th r
  | th <= 30  = [drawArc' (-th) r]
  | otherwise = drawArc' (-30) r : drawArcR (th - 30) r

--
-- | 高速に右回りに円弧を描く
--
quickDrawArcR :: Float
            -> Float
            -> Command
quickDrawArcR th r = [drawArc' (-th) r]

--
-- | 亀の描いた線を元に solid な Polygon を描く
--
drawPolygon :: [Command]        -- ^ Polygon を描くための亀の動き
            -> Command
drawPolygon cs = concat [push, concat cs, pop, [\st -> drawPolygon' st]]
  where
    drawPolygon' st = (Color (penColor st') $ Polygon ps, st')
      where
        (ps, st') = makePoints [] (concat cs) st
        makePoints ps []       st = (reverse (point st : ps), st)
        makePoints ps (c : cs) st = makePoints (point st : ps) cs (snd $ c st)


------------------------------------------------------------
-- * グラフを描くコマンド
------------------------------------------------------------

--
-- | y = f(x) の陽関数のグラフを描く
--
drawGraph :: (Float -> Float)                      -- ^ f(x)
          -> [Float]                               -- ^ 定義域
          -> Command
drawGraph fx domain = drawGraph' (id, fx) domain

--
-- | x = f(t), y = g(t) の陰関数のグラフを描く
--
drawGraph' :: ((Float -> Float), (Float -> Float)) -- ^ (f(t), g(t))
           -> [Float]                              -- ^ 定義域
           -> Command
drawGraph' _        []       = [\st -> (Blank, st)]
drawGraph' (fx, fy) (t : ts) = concat $ cmd1 ++ cmd2
  where
    cmd1 = [pu, goto (fx t, fy t), pd]
    cmd2 = [goto (fx t, fy t) | t <- ts]

--
-- | r = f(th) の極座標方程式のグラフを描く
--
drawPolarGraph :: (Float -> Float)                 -- ^ f(th)
               -> [Float]                          -- ^ 定義域
               -> Command
drawPolarGraph _  []       = [\st -> (Blank, st)]
drawPolarGraph fp (t : ts) = concat $ cmd1 ++ cmd2
  where
    cmd1 = [pu, goto (polarToRectangular (fp t, t)), pd]
    cmd2 = [goto $ polarToRectangular (fp t, t) | t <- ts]


------------------------------------------------------------
-- * 方眼を表示するコマンド
------------------------------------------------------------

--
-- | 方眼を表示する (描画範囲 : -500 ~ 500, 方眼サイズ = 10)
--
grid :: Command
grid = grid' 500 10

--
-- | 方眼を表示する
--
grid' :: Float                  -- ^ 方眼を描く範囲
      -> Float                  -- ^ 方眼の一目盛のサイズ
      -> Command
grid' range size = [\st -> (blueLine1 <> blueLine2 <> redLine, st)]
  where
    blueLine1 = f col [-range, -range + size .. range]
      where col = makeColor 0.5 0.5 1.0 0.2

    blueLine2 = f col [-range, -range + 10 * size .. range]
      where col = makeColor 0.5 0.5 1.0 0.3

    redLine   = f col [0]
      where col = makeColor 1.0 0.5 0.5 0.6

    f col lst = Color col $ Pictures $ concat [[hLine n, vLine n] | n <- lst]
      where
        hLine n = Line [(-range, n), (range, n)]
        vLine n = Line [(n, -range), (n, range)]


------------------------------------------------------------
-- * 亀の状態の更新
------------------------------------------------------------

--
-- | 亀の向きを更新する
--
updateHeading :: (Float -> Float) -- ^ 亀の向きを変化させる関数
            -> Command
updateHeading f = [\st -> (Blank, st { heading = f (heading st) })]

--
-- | 亀の位置を更新する
--
updatePoint :: (Point -> Point) -- ^ 亀の位置を変化させる関数
            -> Command
updatePoint f = [\st -> goto' (f $ point st) st]

--
-- | 線の太さを更新する
--
updateThickness :: (Float -> Float) -- ^ 線の太さを変化させる関数
                -> Command
updateThickness f = [\st -> (Blank, st { thickness = f (thickness st) })]

--
-- | pen の色を更新する
--
updateColor :: (Float -> Float) -- ^ 赤成分を変化させる関数
            -> (Float -> Float) -- ^ 緑成分を変化させる関数
            -> (Float -> Float) -- ^ 青成分を変化させる関数
            -> (Float -> Float) -- ^ アルファ成分を変化させる関数
            -> Command
updateColor fr fg fb fa = [\st -> (Blank, st { penColor = newColor st })]
  where newColor st = makeColor (fr r) (fg g) (fb b) (fa a)
          where (r, g, b, a) = rgbaOfColor $ penColor st


------------------------------------------------------------
-- * その他
------------------------------------------------------------

--
-- | 複数のコマンドの繰り返しを１つのコマンドにする
--
repCommand :: Int               -- ^ 繰り返す回数
           -> [Command]         -- ^ 繰り返すコマンド
           -> Command
repCommand n cLst = concat $ concat $ replicate n cLst


dot :: Command
dot = [dot']

dot' :: PrimitiveCommand
dot' st = (isDraw st $ Translate x y $ ThickCircle 0 0, st)
  where (x, y) = point st
