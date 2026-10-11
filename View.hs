-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures 
        [ tekenVeld (veld gstate) (200),
          color yellow(circleSolid 25)]

tekenVeld :: [[String]] -> Float -> Picture
tekenVeld [] y = Blank
tekenVeld input@(h:t) y = pictures [tekenLine h (-200) y, tekenVeld t (y-30)]

tekenLine :: [String] -> Float -> Float -> Picture
tekenLine [] x y = Blank
tekenLine input@(h:t) x y | h=="w"        = pictures [translate x y (color blue(rectangleSolid 30 30)), tekenLine t (x+30) y]
                          | h=="o"        = pictures [translate x y (color yellow(circleSolid 5)), tekenLine t (x+30) y]
                          | otherwise     = tekenLine t (x+30) y
