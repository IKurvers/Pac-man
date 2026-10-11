-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: Picture -> GameState -> IO Picture
view plaatje = return . viewPure plaatje

viewPure :: Picture -> GameState -> Picture
viewPure pacmanZelf gstate = pictures 
        [ tekenVeld (veld gstate) (280),
          translate (-20) 20 (scale 0.07 0.07 pacmanZelf)]

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
