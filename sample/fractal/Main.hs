--
-- | L-system によるフラクタルな図形
--

module Main where

import Graphics.Gloss
import L_system
import Turtle

main :: IO ()
main = display window white pic
  where
    window = InWindow "Fractal" (800, 600) (10, 10)
    pic    = Pictures
      [ Color black $ Line [(-380, 0), (380, 0)] <> Line [(0, -280), (0, 280)]
      , Translate (-200)   150  $ Scale 0.5 0.5 $ kochCurve 3
      , Translate   200    150  $ Scale 0.5 0.5 $ hilbertCurve 5
      , Translate (-200) (-150) $ Scale 0.5 0.5 $ sierpinskiTriangle 8
      , Translate   200  (-150) $ Scale 0.5 0.5 $ tree 5 ]


-- | コッホ曲線
kochCurve :: Int -> Picture
kochCurve n = drawLine st (size / 3 ^ n) 60 string
  where
    size   = 500
    st     = initST {point = (- size / 2, size / 2 / sqrt 3), penColor = blue}
    axiom  = "F--F--F"                                  -- 初期文字列
    rules  = [('F', "F+F--F+F")]                        -- 変換規則
    string = l_system axiom rules n                     -- n : 繰り返し回数

-- | ヒルベルト曲線
hilbertCurve :: Int -> Picture
hilbertCurve n = drawLine st (size / (2 ^ n)) 90 string
  where
    size   = 500
    st     = initST {point = (- size / 2, - size / 2), penColor = rose}
    string = l_system "L" [('L', "+RF-LFL-FR+"), ('R', "-LF+RFR+FL-")] n

-- | シェルピンスキーのギャスケット
sierpinskiTriangle :: Int -> Picture
sierpinskiTriangle n = drawLine st (size / 2 ^ n) 60 string
  where
    size   = 500
    st     = initST {point = (- size / 2, - size * sqrt 3 / 4), penColor = red}
    string = l_system "RF" [('L', "-RF+LF+RF-"), ('R', "+LF-RF-LF+"), ('F', "")] n

sierpinskiTriangle2 :: Int -> Picture
sierpinskiTriangle2 n = drawLine st (size / 2 ^ n) 60 string
  where
    size = 250
    st   = initST {point = (size, - size * sqrt 3 / 2), angle = 180}
    axiom = "FXF--FF--FF"
    rules = [('F', "FF"), ('X', "--FXF++FXF++FXF--")]
    string = l_system axiom rules n

-- | スタックを使用した木
tree :: Int -> Picture
tree n = drawLine st (size / 2.17 ^ n) 20 string
  where
    size   = 400
    st     = initST {angle = 90, point = (0, -250), penColor = green}
    string = l_system "X" [('X', "F[+X]F[-X]+X"), ('F', "FF")] n
