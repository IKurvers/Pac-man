-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures 
        [ draw,
          color yellow(circleSolid 10)]
{-
drawWall :: Float -> Float -> Float -> Float -> Picture
drawWall x y b h = color blue(translate x y (rectangleWire b h))
-}
draw = do
        input <- readFile "veld_pac-man.txt" 
        teken . map read . words $ input

teken :: [String] -> Picture
teken input@(h:t) = 