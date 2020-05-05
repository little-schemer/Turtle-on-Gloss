------------------------------------------------------------
-- | Dragon 曲線 (再帰版)
------------------------------------------------------------

import           Graphics.Gloss
import           Graphics.Turtle
import           Options.Applicative


-- | パラメータ
size  = 300 :: Float


-- | コマンドオプション
data Option = Option { dLevel :: Int }

opt :: Parser Option
opt = Option <$> option auto (short 'L' <> value 8)


-- | Main
main :: IO ()
main = do
  Option level <- execParser (info opt mempty)
  runTurtle window (greyN 0.3) 100 [(s, cmd level) | s <- [st1, st2, st3, st4]]
  where
    window = initWindow {title = "Dragon Curve"}
    st1 = initST {mark = False, penColor = red,    angle =  45}
    st2 = initST {mark = False, penColor = green,  angle = 135}
    st3 = initST {mark = False, penColor = blue,   angle = 225}
    st4 = initST {mark = False, penColor = yellow, angle = 315}
    cmd level = [dragonCurve level size]


-- | 再帰による Dragon 曲線
dragonCurve :: Int -> Float -> Command
dragonCurve n len = dR n len
  where
    dR 0 len = qf len
    dR n len = concat [ql 45, dR n' len', qr 90, dL n' len', ql 45]
      where (n', len') = (n - 1, len / sqrt 2)

    dL 0 len = qf len
    dL n len = concat [qr 45, dR n' len', ql 90, dL n' len', qr 45]
      where (n', len') = (n - 1, len / sqrt 2)
