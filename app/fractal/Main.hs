module Main where

import Graphics.Gloss
import L_system


main :: IO ()
main = display window white pic
  where
    window = InWindow "Fractal" (800, 600) (10, 10)
    pic    = Pictures
      [ Color red $ grid
      , Translate (-200)   150  $ Scale 0.5 0.5 $ kochCurve 3
      , Translate   200    150  $ Scale 0.5 0.5 $ hilbertCurve 5
      , Translate (-200) (-150) $ Scale 0.5 0.5 $ sierpinskiTriangle 6
      , Translate   200  (-150) $ Scale 0.5 0.5 $ tree 5 ]


grid :: Picture
grid = Pictures [ Line [(-380, 0), (380, 0)], Line [(0, -280), (0, 280)] ]

-- コッホ曲線
kochCurve :: Int -> Picture
kochCurve n = drawLine initST (size / 3 ^ n) 60 string
  where
    size   = 500
    initST = (0, (- size / 2, size / 2 / sqrt 3), blue) -- 亀の初期状態
    axiom  = "F--F--F"                                  -- 初期文字列
    rules  = [('F', "F+F--F+F")]                        -- 変換規則
    string = l_system axiom rules n                     -- n : 繰り返し回数

-- ヒルベルト曲線
hilbertCurve :: Int -> Picture
hilbertCurve n = drawLine initST (size / (2 ^ n)) 90 string
  where
    size   = 500
    initST = (0, (- size / 2, - size / 2), cyan)
    string = l_system "L" [('L', "+RF-LFL-FR+"), ('R', "-LF+RFR+FL-")] n

-- シェルピンスキーのギャスケット
sierpinskiTriangle :: Int -> Picture
sierpinskiTriangle n = drawLine initST (size / 2 ^ (n - 1)) 120 string
  where
    size   = 500
    initST = (0, (- size / 2, - size * sqrt 3 / 4), black)
    string = l_system "F" [('F', "F+F+F+ff"), ('f', "ff")] n

-- スタックを使用した木
tree :: Int -> Picture
tree n = drawLine initST (size / 2.17 ^ n) 20 string
  where
    size   = 400
    initST = (90, (0, -250), green)
    string = l_system "X" [('X', "F[+X]F[-X]+X"), ('F', "FF")] n
