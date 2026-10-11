-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> IO Picture
view pac g1 g2 g3 g4 = return . viewPure pac g1 g2 g3 g4

viewPure :: Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> Picture
viewPure pacmanZelf geest1 geest2 geest3 geest4 gstate = pictures 
        [ tekenVeld (veld gstate) (280),
          translate (0) 40 (scale 0.07 0.07 pacmanZelf),
          translate (-40) 20 (scale 0.07 0.07 geest1),
          translate (-10) 20 (scale 0.07 0.07 geest2),
          translate (20) 20 (scale 0.07 0.07 geest3),
          translate (50) 20 (scale 0.07 0.07 geest4)]

tekenVeld :: [[String]] -> Float -> Picture
tekenVeld [] y = Blank
tekenVeld input@(h:t) y = pictures [tekenLine h (-270) y, tekenVeld t (y-20)]

tekenLine :: [String] -> Float -> Float -> Picture
tekenLine [] x y = Blank
tekenLine input@(h:t) x y | h=="w"        = pictures [translate x y (color blue(rectangleSolid 20 20)), tekenLine t (x+20) y]
                          | h=="o"        = pictures [translate x y (color yellow(circleSolid 5)), tekenLine t (x+20) y]
                          | h=="l"        = pictures [translate (x-5) y (color blue(rectangleSolid 5 25)), tekenLine t (x+20) y]
                          | h=="r"        = pictures [translate (x+5) y (color blue(rectangleSolid 5 25)), tekenLine t (x+20) y]
                          | h=="_"        = pictures [translate x (y-10) (color blue(rectangleSolid 40 5)), tekenLine t (x+20) y]
                          | h=="-"        = pictures [translate x (y+10) (color blue(rectangleSolid 40 5)), tekenLine t (x+20) y]
                          | h=="n"        = tekenLine t (x+20) y
                          | h=="d"        = pictures [translate (x+5) (y+10) (color magenta(rectangleSolid 20 5)), tekenLine t (x+20) y]
                          | otherwise     = tekenLine t (x+20) y
