module View where

import Graphics.Gloss
import Model
import Controller
import System.Random

data Pict = Pict
  { pacmanUp    :: Picture
  , pacmanDown  :: Picture
  , pacmanLeft  :: Picture
  , pacmanRight :: Picture
  }

getPacmanPicture :: Pict -> PacPict -> Picture
getPacmanPicture pics p = case p of
    PacU -> pacmanUp pics
    PacD -> pacmanDown pics
    PacL -> pacmanLeft pics
    PacR -> pacmanRight pics

view :: StdGen -> Pict -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> IO Picture
view gen pics g1 g2 g3 g4 k h gstate = do
  return(viewPure pics g1 g2 g3 g4 k h gstate gen)

-- levens, score en highscore toegevoegd
viewPure :: Pict -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> StdGen -> Picture
viewPure pics geest1 geest2 geest3 geest4 kers hart gstate gen = pictures
        ([ fst (tekenVeld kers (veld gstate) 245 gen),
          drawPlayer pics (player gstate),
          pictures (map drawGhost (ghosts gstate)),
          translate (-285) 292 (scale 0.2 0.2 (color white(Text "Lives:"))),
          translate (90) 292 (scale 0.2 0.2 (color white(Text "Highscore:"))),
          translate (220) 292 (scale 0.2 0.2 (color white(Text (show (highscore gstate))))),
          translate (0) 292 (scale 0.2 0.2 (color white(Text (show (score gstate)))))] 
          ++ 
          (drawLives (lives gstate) hart (-200)))
  where
    drawGhost ghost = translate x y (scale 0.06 0.06 (ghostImage ghost))
      where
        (col, row) = ghostPosition ghost
        (x, y) = gridToScreen col row
        ghostImage g = case ghostName g of
          Blinky -> geest1
          Pinky -> geest2
          Inky -> geest3
          Clyde -> geest4

drawPlayer :: Pict -> Player -> Picture
drawPlayer pics p = translate x y (scale 0.06 0.06 image)
  where
    (col, row) = smoothCell p
    (x, y) = gridToScreen col row
    image = getPacmanPicture pics (pict p)

drawLives :: Float -> Picture -> Float -> [Picture]
drawLives 0 _ _ = []
drawLives n hart x = ((translate x 300 (scale 0.08 0.08 hart)): drawLives (n-1) hart (x+30))

gridToScreen :: Float -> Float -> (Float, Float)
gridToScreen col row = (-270 + col * 20, 245 - row * 20)

tekenVeld :: Picture -> [[String]] -> Float -> StdGen -> (Picture, StdGen)
tekenVeld _ [] y g = (Blank,g)
tekenVeld fruit (h:t) y g = (pictures [r1, r2],g'')
                                    where (r1,g') = tekenLine fruit h (-270) y g
                                          (r2,g'') = tekenVeld fruit t (y-20) g'

tekenLine :: Picture -> [String] -> Float -> Float -> StdGen -> (Picture,StdGen)
tekenLine _ [] x y g = (Blank,g)
tekenLine fruit (h:t) x y g       | h=="w"        = (pictures [translate x y (color blue(rectangleSolid 15 15)), r2],g')
                                  | h=="o"        = (pictures [translate x y (f), r2],g'')
                                  | h=="0"        = (pictures [translate x y (color yellow(circleSolid 5)), r2],g')
                                  | h=="l"        = (pictures [translate x y (color blue(rectangleSolid 5 20)), r2],g')
                                  | h=="r"        = (pictures [translate x y (color blue(rectangleSolid 5 20)), r2],g')
                                  | h=="_"        = (pictures [translate x (y-5) (color blue(rectangleSolid 40 5)), r2],g')
                                  | h=="-"        = (pictures [translate x (y+5) (color blue(rectangleSolid 40 5)), r2],g')
                                  | h=="n"        = (r2,g')
                                  | h=="d"        = (pictures [translate (x+5) (y+5) (color magenta(rectangleSolid 20 5)), r2],g')
                                  | otherwise     = (r2,g')
                                     where (f,g'') = randomFruit fruit g
                                           (r2,g') =  tekenLine fruit t (x+20) y g''

randomFruit :: Picture -> StdGen -> (Picture, StdGen)
randomFruit fruit g | r <= 97   = (color yellow(circleSolid 2), g')
                     | otherwise = (scale 0.06 0.06 fruit, g')
                        where (r,g') = randomR(1 :: Int, 100 :: Int) g

