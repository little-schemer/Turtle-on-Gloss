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


data TurtleST = TurtleST { angle    :: Float -- ^ 亀の向き
                         , point    :: Point -- ^ 亀の位置
                         , penColor :: Color -- ^ ペンの色
                         , pen      :: Bool  -- ^ up or down
                         , mark     :: Bool  -- ^ 亀のマーク
                         , stack    :: [(Float, Point, Color, Bool, Bool)]
                         } deriving Show

type PrimitiveCommand  = TurtleST -> (Picture, TurtleST)
type Command           = [PrimitiveCommand]
type Model             = (Picture, [(TurtleST, Command)])



--------------------------------------------------
-- * 初期値の雛形
--------------------------------------------------

--
-- | TurtleST の初期値を設定する
--
-- > 《初期値》l
-- >   + angle    = 0
-- >   + point    = (0, 0)
-- >   + penColor = black
-- >   + pen      = True
-- >   + mark     = True
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
-- | Display の初期値を設定する
--
-- > 《初期値》
-- >   + Window のタイトル   : "Turtle Graphics"
-- >   + Window のサイズ     : (800, 600)
-- >   + Window のポジション : (10, 10)
--
initDisp :: Display
initDisp = InWindow "Turtle Graphics" (800, 600) (10, 10)



---------------------------------------------------
-- * 画像表示
---------------------------------------------------

--
-- | 亀に図形を描かせる
--
runTurtle :: Display                 -- ^ 描画モード
          -> Color                   -- ^ 背景色
          -> Int                     -- ^ 1 秒あたりのステップ数
          -> [(TurtleST, [Command])] -- ^ (亀の初期値とコマンド) のリスト
          -> IO ()
runTurtle disp c step tds = simulate disp c step model drawModel simModel
  where
    model = (Blank, map (\(st, lst) -> (st, concat lst)) tds)

    drawModel (pic, ts) = pic <> (Pictures $ map (f . fst) ts)
      where
        turtleMark = Polygon [(0, -3), (0, 3), (8, 0)]
        f st = if (mark st)
               then Translate x y $ Rotate th $ Color c $ turtleMark
               else Blank
          where
            (x, y)  = point st
            (th, c) = (360 - angle st, penColor st)

    simModel _ _ (pic, [])  = (pic, [])
    simModel _ _ (pic, ts) = foldl f (pic, []) ts
      where
        f model (_, [])            = model
        f (pic, ts) (st, cmd : cs) = (p <> pic, (st', cs) : ts)
          where (p, st') = cmd st

--
-- | 最終結果だけを表示する
--
dispPicture :: Display                 -- ^ 描画モード
            -> Color                   -- ^ 背景色
            -> [(TurtleST, [Command])] -- ^ (亀の初期値とコマンド) のリスト
            -> IO ()
dispPicture disp c tds = display disp c $ Pictures $ map makePicture tds
  where makePicture (st, cmds) = fst $ foldl f (Blank, st) (concat cmds)
          where f (pic, st) cmd = let (pic', st') = cmd st in (pic <> pic', st')



---------------------------------------------------
-- * 補助関数
---------------------------------------------------

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



---------------------------------------------------
-- * 基本コマンド
---------------------------------------------------

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

-- | p の位置へ移動する（亀の向きは不変。 pen == True なら線を描く）
goto :: Point -> Command
goto p = [toPoint p]

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



--------------------------------------------------
-- * Alias
--------------------------------------------------

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



--------------------------------------------------
-- * 図形を描くコマンド
--------------------------------------------------

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
  where drawCircle' r st = (isDraw st $ Translate x y $ Circle r, st)
          where (x, y) = point st



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



--------------------------------------------------
-- * その他
--------------------------------------------------

--
-- | グリッドを表示する
--
grid :: Command
grid = [\st -> (line1 <> line2 <> line3, st)]
  where
    red'  = makeColorI 200 80 80 200
    blue' = makeColorI 150 150 200 150
    line1 = Color red'  $ Line [(-500, 0), (500, 0)]
    line2 = Color red'  $ Line [(0, -500), (0, 500)]
    line3 = Color blue' $ Pictures $ concatMap f [-500, -490 .. 500]
      where f n = [Line [(-500, n), (500, n)], Line [(n, -500), (n, 500)]]

--
-- | 複数のコマンドの繰り返しを１つのコマンドにする
--
repCommand :: Int               -- ^ 繰り返す回数
           -> [Command]         -- ^ 繰り返すコマンド
           -> Command
repCommand n cLst = concat $ concat $ replicate n cLst
