-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import System.Random

view :: StdGen -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> IO Picture
view gen pac g1 g2 g3 g4 f gstate = do
  return(viewPure pac g1 g2 g3 g4 f gstate gen)

viewPure :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> StdGen -> Picture
viewPure pacmanZelf geest1 geest2 geest3 geest4 fruit gstate gen = pictures 
        [ fst (tekenVeld fruit (veld gstate) (280) gen),
          translate (-10) 60 (scale 0.06 0.06 pacmanZelf),
          translate (-40) 20 (scale 0.06 0.06 geest1),
          translate (-10) 20 (scale 0.06 0.06 geest2),
          translate (20) 20 (scale 0.06 0.06 geest3),
          translate (50) 20 (scale 0.06 0.06 geest4)]

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