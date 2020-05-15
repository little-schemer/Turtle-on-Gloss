------------------------------------------------------------
-- | Koch 曲線 (再帰版)
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle
import           Options.Applicative


-- | パラメータ
size  = 400 :: Float


-- | コマンドオプション
data Option = Option { kochLevel :: Int }

opt :: Parser Option
opt = Option <$> option auto (short 'L' <> value 4 <> metavar "Int")


-- | Main
main :: IO ()
main = do
  Option level <- execParser (info opt mempty)
  runTurtle window white 100 [(s, cmd level) | s <- [st1, st2, st3]]
  where
    window = initWindow {title = "Koch Curve"}
    st1 = initST {angle =    0, point = (-200,  200 / sqrt 3), mark = False}
    st2 = initST {angle = -120, point = ( 200,  200 / sqrt 3), mark = False}
    st3 = initST {angle =  120, point = (   0, -400 / sqrt 3), mark = False}
    cmd level = [kochCurve level size]


-- | 再帰による Koch 曲線
kochCurve :: Int -> Float -> Command
kochCurve 0 len = qf len
kochCurve n len = concat [kh, ql 60, kh, qr 120, kh, ql 60, kh]
  where kh = kochCurve (n - 1) (len / 3)
