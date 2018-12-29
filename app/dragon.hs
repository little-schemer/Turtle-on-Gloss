import Graphics.Gloss
import Turtle

dragonR :: Float -> Int -> [Command]
dragonR len 0 = [forward len]
dragonR len n = lt ++ dR ++ rt ++ dL ++ lt
  where
    lt = [left 45]
    rt = [right 90]
    dR = dragonR (len / sqrt 2) (n - 1)
    dL = dragonL (len / sqrt 2) (n - 1)

dragonL :: Float -> Int -> [Command]
dragonL len 0 = [forward len]
dragonL len n = rt ++ dR ++ lt ++ dL ++ rt
  where
    lt = [left 90]
    rt = [right 45]
    dR = dragonR (len / sqrt 2) (n - 1)
    dL = dragonL (len / sqrt 2) (n - 1)


main :: IO ()
main = display window white pic
    where
      window = InWindow "Turtle" (800, 600) (10, 10)
      (_, pic) = runTurtle (dragonR 400 10) initST { point = (-200, 0)}

      -- st = TurtleST {angle = 0, point = (0, 0), penColor = white, pen = True}
      -- cs = (left 9) : (concat $ replicate 12 [drawPolygonL 20 30, left 30])
      -- (_, pic) = runTurtle cs st
