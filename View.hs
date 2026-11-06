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
  , pacmanUphalf    :: Picture
  , pacmanDownhalf  :: Picture
  , pacmanLefthalf  :: Picture
  , pacmanRighthalf :: Picture
  , pacmanHeel  :: Picture
  }

getPacmanPicture :: Pict -> PacPict -> Picture
getPacmanPicture pics p = case p of
    PacU -> pacmanUp pics
    PacD -> pacmanDown pics
    PacL -> pacmanLeft pics
    PacR -> pacmanRight pics
    PacUh -> pacmanUphalf pics
    PacDh -> pacmanDownhalf pics
    PacLh -> pacmanLefthalf pics
    PacRh -> pacmanRighthalf pics
    Pacheel -> pacmanHeel pics

view :: Pict -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> IO Picture
view pics g1 g2 g3 g4 c s a r m h o dood gstate = return (viewPure pics g1 g2 g3 g4 c s a r m h o dood gstate )

viewPure :: Pict -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> GameState -> Picture
viewPure pics geest1 geest2 geest3 geest4 cherry strawberry apple orange melon hart oogjes dood gstate = pictures 
      ([ tekenVeld [cherry, strawberry, apple, orange, melon] (veld gstate) 245
      , drawPlayer pics (player gstate)
      , pictures (map drawGhost (ghosts gstate))
      , translate (-285) 292 (scale 0.2 0.2 (color white (Text "Lives:")))
      , translate (90) 292 (scale 0.2 0.2 (color white (Text "Highscore:")))
      , translate (220) 292 (scale 0.2 0.2 (color white (Text (show (highscore gstate)))))
      , translate (0) 292 (scale 0.2 0.2 (color white (Text (show (score gstate)))))
      ]
      ++ concat [drawLives (lives gstate) hart (-200)]
      ++ concat [if died gstate then
            [translate (-200) (0) (scale 0.3 0.3 (color white (Text "Game Over,"))),
            translate (-200) (-50) (scale 0.3 0.3 (color white (Text "Press (r) to restart")))]
          else []]
      ++ concat [if paused gstate && not (died gstate) then
            [translate (-65) (-30) (scale 0.3 0.3 (color white (Text "Paused")))]
          else []]
      )
    

  where
    drawGhost ghost =
      translate x y (scale 0.06 0.06 (ghostImage ghost))
      where
        (col, row) = ghostPosition ghost
        (x, y) = gridToScreen col row
        ghostImage g
          | ghostMode g == Eyes   = oogjes
          | hunting g > 0         = dood
          | otherwise = case ghostName g of
              Blinky -> geest1
              Pinky  -> geest2
              Inky   -> geest3
              Clyde  -> geest4

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

tekenVeld :: [Picture] -> [[String]] -> Float -> Picture
tekenVeld _ [] _ = Blank
tekenVeld fruit (h:t) y = pictures[ tekenLine fruit h (-270) y, tekenVeld fruit t (y-20)]

tekenLine :: [Picture] -> [String] -> Float -> Float -> Picture
tekenLine _ [] _ _ = Blank
tekenLine fruit (h:t) x y = pictures[ tekenCel fruit h x y, tekenLine fruit t (x+20) y]

tekenCel :: [Picture] -> String -> Float -> Float -> Picture
tekenCel fruit h x y  | h == "w" = translate x y (color blue (rectangleSolid 15 15))
                      | h == "o" = translate x y (color yellow (circleSolid 2))
                      | h == "0" = translate x y (color yellow (circleSolid 5))
                      | h == "l" = translate x y (color blue (rectangleSolid 5 20))
                      | h == "r" = translate x y (color blue (rectangleSolid 5 20))
                      | h == "_" = translate x (y-5) (color blue (rectangleSolid 40 5))
                      | h == "-" = translate x (y+5) (color blue (rectangleSolid 40 5))
                      | h == "n" = Blank
                      | h == "d" = translate (x+5) (y+5) (color magenta (rectangleSolid 20 5))
                      | h == "c" = tekenFruit 0
                      | h == "s" = tekenFruit 1
                      | h == "a" = tekenFruit 2
                      | h == "R" = tekenFruit 3
                      | h == "m" = tekenFruit 4
                      | otherwise = Blank
                      where tekenFruit i = translate x y (scale 0.06 0.06 (fruit !! i))
