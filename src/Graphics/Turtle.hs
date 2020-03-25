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


import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Geometry.Angle



------------------------------------------------------------
-- * 型
------------------------------------------------------------

-- | 亀の状態
data TurtleST = TurtleST { angle    :: Float -- ^ 亀の向き
                         , point    :: Point -- ^ 亀の位置
                         , penColor :: Color -- ^ ペンの色
                         , pen      :: Bool  -- ^ up or down
                         , mark     :: Bool  -- ^ 亀のマーク
                         , stack    :: [(Float, Point, Color, Bool, Bool)]
                         } deriving Show

-- | 画面の設定
data WinConfig = WinConfig { title   :: String         -- ^ Window のタイトル
                           , winSize :: (Int, Int)     -- ^ Window のサイズ
                           , winPos  :: (Int, Int)     -- ^ Window の位置
                           , zoom    :: Float          -- ^ 画像の拡大率
                           , shiftXY :: (Float, Float) -- ^ 画像の移動量
                           } deriving Show

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
initST = TurtleST { angle    = 0
                  , point    = (0, 0)
                  , penColor = black
                  , pen      = True
                  , mark     = True
                  , stack    = []
                  }

--
-- | WinConfig の初期値を設定する
--
initWindow :: WinConfig
initWindow = WinConfig { title   = "Turtle Graphics"
                       , winSize = (800, 600)
                       , winPos  = (10, 10)
                       , zoom    = 1
                       , shiftXY = (0, 0)
                       }



------------------------------------------------------------
-- * 画像表示
------------------------------------------------------------

--
-- | 亀に図形を描かせる
--
runTurtle :: WinConfig               -- ^ 画面の状態
          -> Color                   -- ^ 背景色
          -> Int                     -- ^ 1 秒あたりのステップ数
          -> [(TurtleST, [Command])] -- ^ [ ( 亀の初期値, [ コマンド ] ) ]
          -> IO ()
runTurtle window bc step tds = simulate disp bc step model drawModel simModel
  where
    -- 描画モード
    disp = InWindow (title window) (winSize window) (winPos window)

    -- モデルの初期値
    model = (Blank, map (\(st, lst) -> (st, concat lst)) tds)

    -- モデルを描画する
    drawModel (pic, ts) = pic' <> turtleMarks
      where
        (z, (sx, sy)) = (zoom window, shiftXY window)

        pic'        = Scale z z $ Translate sx sy pic
        turtleMarks = Pictures $ map (dispMark . fst) ts

        dispMark st = if (mark st)
                      then Translate x' y' $ Rotate th $ tMark
                      else Blank
          where
            (th, c, (x, y)) = (360 - angle st, penColor st, point st)
            (x', y') = ((x + sx) * z, (y + sy) * z)
            tMark = (Color white mark1) <> (Color c mark2)
              where
                mark1 = Polygon [(0, -5), (0, 5), (12, 0)]
                mark2 = Polygon [(1, -4), (1, 4), (10, 0)]

    -- モデルを変化させる
    simModel _ _ (pic, [])  = (pic, [])
    simModel _ _ (pic, ts) = foldl f (pic, []) ts
      where
        f model (_, [])            = model
        f (pic, ts) (st, cmd : cs) = (pic <> pic', (st', cs) : ts)
          where (pic', st') = cmd st

--
-- | 最終結果だけを表示する
--
dispPicture :: WinConfig               -- ^ 画面の状態
            -> Color                   -- ^ 背景色
            -> [(TurtleST, [Command])] -- ^ (亀の初期値とコマンド) のリスト
            -> IO ()
dispPicture window c tds = display disp c $ Scale z z $ Translate sx sy pic
  where
    disp = InWindow (title window) (winSize window) (winPos window)

    (z, (sx, sy)) = (zoom window, shiftXY window)

    pic = Pictures $ map makePicture tds
    makePicture (st, cmds) = fst $ foldl f (Blank, st) (concat cmds)
          where f (pic, st) cmd = let (pic', st') = cmd st in (pic <> pic', st')



------------------------------------------------------------
-- * 補助関数
------------------------------------------------------------

-- | 移動先のポイントを求める
newPoint :: Float -> Float -> Point -> Point
newPoint n th p = p PA.+ n PA.* (unitVectorAtAngle $ degToRad th)

-- | p の位置へ移動する（亀の向きは不変。 pen == True なら線を描く）
toPoint :: Point -> PrimitiveCommand
toPoint p st = (isDraw st $ Line [point st, p], st {point = p})

-- | n だけ前進する (pen == True なら線を描く)
move :: Float -> PrimitiveCommand
move n st = toPoint (newPoint n th p) st
  where (th, p) = (angle st, point st)

-- | th 度旋回する (th > 0 : 左旋回, th < 0 : 右旋回)
turn :: Float -> PrimitiveCommand
turn th st = (Blank, st {angle = normalize $ th + angle st})

-- | 角度を 0 <= th < 360 に正規化する
normalize :: Float -> Float
normalize th
  | th >= 360 = normalize (th - 360)
  | th <    0 = normalize (th + 360)
  | otherwise = th

-- | pen == True なら図形を描く
isDraw :: TurtleST -> Picture -> Picture
isDraw st pic = if pen st then Color (penColor st) $ pic else Blank



------------------------------------------------------------
-- * 基本コマンド
------------------------------------------------------------

-- | n だけ前進する (pen == True なら線を描く)
forward :: Float -> Command
forward n
  | n <= 50   = [move n]
  | otherwise = move 50 : forward (n - 50)

-- | 高速に前進する
quickForward :: Float -> Command
quickForward n = [move n]

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

-- | 高速に左旋回する
quickLeft :: Float -> Command
quickLeft th = [turn th]

-- | 高速に右旋回する
quickRight :: Float -> Command
quickRight th = [turn (-th)]

-- | p の位置へ移動する（pen == True なら線を描く）
goto :: Point -> Command
goto (x, y) = [goto']
  where goto' st = toPoint (x, y) st {angle = radToDeg $ argV (x - x', y - y')}
          where (x', y') = point st

-- | 亀の向きを設定する
setAngle :: Float -> Command
setAngle th = [\st -> (Blank, st {angle = normalize th})]

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

-- | 亀の状態を Push する
push :: Command
push = [\st -> (Blank, st {stack = f st : stack st})]
  where f st = (angle st, point st, penColor st, pen st, mark st)

-- | 亀の状態を Pop する
pop :: Command
pop = [\st -> (Blank, f (stack st))]
  where
    f ((a, p, c, p', m) : sk) = TurtleST { angle    = a
                                         , point    = p
                                         , penColor = c
                                         , pen      = p'
                                         , mark     = m
                                         , stack    = sk
                                         }

-- | 何もしない
nop :: Command
nop = [\st -> (Blank, st)]

nopN :: Int -> Command
nopN n = concat $ replicate n nop



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

-- | right
rt = right

-- | quickLeft
ql = quickLeft

-- | quickRight
qr = quickRight

-- | penUp
pu = penUp

-- | penDown
pd = penDown



------------------------------------------------------------
-- * 図形を描くコマンド
------------------------------------------------------------

----------------------------------------
-- ** 正多角形
----------------------------------------

--
-- | 一辺の長さが m の正 n 角形を左回りに描く
--
drawPolygonL :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> Command
drawPolygonL = drawPolygon left

--
-- | 一辺の長さが m の正 n 角形を右回りに描く
--
drawPolygonR :: Int             -- ^ 角数
             -> Float           -- ^ 一辺の長さ
             -> Command
drawPolygonR = drawPolygon right

--
-- | 正多角形を描く
--
drawPolygon :: (Float -> Command) -> Int -> Float -> Command
drawPolygon cmd n m = concat $ cs ++ [cmd (- th / 2)]
  where
    th = 360 / (fromIntegral n)
    cs = [cmd (th / 2), (repCommand n [forward m, cmd th])]



----------------------------------------
-- ** 円
----------------------------------------

--
-- | 亀の位置を中心に、半径 r の円を描く
--
drawCircle :: Float             -- ^ 半径
           -> Command
drawCircle r = [drawCircle' r]
  where drawCircle' r st = (Color c $ Translate x y $ Circle r, st)
          where ((x, y), c) = (point st, penColor st)

--
-- | 亀の位置を中心に、半径 r の solid な円を描く
--
drawCircleSolid :: Float        -- ^ 半径
           -> Command
drawCircleSolid r = [drawCircleSolid' r]
  where drawCircleSolid' r st = (Color c $ Translate x y $ circleSolid r, st)
          where ((x, y), c) = (point st, penColor st)



----------------------------------------
-- ** 円弧
----------------------------------------

--
-- | 中心角 th 半径 r の円弧を左回りに描く
--
drawArcL :: Float               -- ^ 中心角
         -> Float               -- ^ 半径
         -> Command
drawArcL th r
  | th <= 10  = [drawArcL' th r]
  | otherwise = drawArcL' 10 r : drawArcL (th - 10) r
  where
    drawArcL' th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
      where
        a  = angle st
        a' = a - 90
        (ox, oy) = newPoint r (a + 90) (point st)
        st' = st {angle = a + th, point = newPoint r (a' + th) (ox, oy)}

--
-- | 中心角 th 半径 r の円弧を右回りに描く
--
drawArcR :: Float               -- ^ 中心角
         -> Float               -- ^ 半径
         -> Command
drawArcR th r
  | th <= 10  = [drawArcR' th r]
  | otherwise = drawArcR' 10 r : drawArcR (th - 10) r
  where
    drawArcR' th r st = (Translate ox oy $ isDraw st $ Arc a' (a' + th) r, st')
      where
        a  = angle st
        a' = 90 + a - th
        (ox, oy) = newPoint r (a - 90) (point st)
        st' = st {angle = a - th, point = newPoint r a' (ox, oy)}



------------------------------------------------------------
-- * グラフ
------------------------------------------------------------

--
-- | 陽関数のグラフを描く
--
drawGraph :: (Float -> Float)   -- ^ 関数 y = f(x)
          -> [Float]            -- ^ 定義域
          -> Command
drawGraph fx domain = drawGraph' (id, fx) domain

--
-- | 陰関数のグラフを描く
--
drawGraph' :: ((Float -> Float), (Float -> Float)) -- ^ (x = f(t), y = g(t))
           -> [Float]                              -- ^ 定義域
           -> Command
drawGraph' (fx, fy) (t : ts) = concat $ cmd1 ++ cmd2
  where
    cmd1 = [pu, goto (fx t, fy t), pd]
    cmd2 = [goto (fx t, fy t) | t <- ts]

--
-- | 極座標方程式のグラフを描く
--
drawPolarGraph :: (Float -> Float) -- ^ 関数 r = f(th)
               -> [Float]          -- ^ 定義域
               -> Command
drawPolarGraph fp (t : ts) = concat $ cmd1 ++ cmd2
  where
    cmd1 = [pu, goto (polarToRectangular (fp t, t)), pd]
    cmd2 = [goto $ polarToRectangular (fp t, t) | t <- ts]



------------------------------------------------------------
-- * 方眼
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
    blueLine1 = Color c $ f [-range, -range + size      .. range]
      where c = makeColor 0.5 0.5 1.0 0.2

    blueLine2 = Color c $ f [-range, -range + 10 * size .. range]
      where c = makeColor 0.5 0.5 1.0 0.3

    redLine   = Color c $ f [0]
      where c = makeColor 1.0 0.5 0.5 0.6

    f lst = Pictures $ concat [[horizontal n, vertical n] | n <- lst]
      where
        horizontal n = Line [(-range, n), (range, n)]
        vertical   n = Line [(n, -range), (n, range)]



------------------------------------------------------------
-- * 亀の状態の更新
------------------------------------------------------------

--
-- | 亀の向きを更新する
--
updateAngle :: (Float -> Float) -> Command
updateAngle f = [\st -> (Blank, st {angle = f (angle st)})]

--
-- | 位置を更新する
--
updatePoint :: (Point -> Point) -> Command
updatePoint f = [\st -> toPoint (f $ point st) st]

--
-- | pen の色を更新する
--
updateColor :: (Float -> Float) -- ^ 赤成分を変化させる関数
            -> (Float -> Float) -- ^ 緑成分を変化させる関数
            -> (Float -> Float) -- ^ 青成分を変化させる関数
            -> (Float -> Float) -- ^ アルファ成分を変化させる関数
            -> Command
updateColor fr fg fb fa = [\st -> (Blank, st {penColor = newColor st})]
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

--
-- | 極座標 -> 直交座標
--
polarToRectangular :: (Float, Float) -> (Float, Float)
polarToRectangular (r, th) = (r * cos th, r * sin th)
