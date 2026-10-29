module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Random

main :: IO ()
main = do
    tekstinput <- readFile "veld_pac-man.txt"
    let beginveld = map words (lines tekstinput)
    let beginstaat = initialState beginveld
    
    gen <- getStdGen

    pacmanZelf <- loadJuicyPNG "pacman-zelf.png"
    geest1 <- loadJuicyPNG "geest1.png"
    geest2 <- loadJuicyPNG "geest2.png"
    geest3 <- loadJuicyPNG "geest3.png"
    geest4 <- loadJuicyPNG "geest4.png"
    fruit <- loadJuicyPNG "fruit.png"
    case (pacmanZelf,geest1,geest2,geest3,geest4,fruit) of
        (Just pac, Just g1,Just g2,Just g3,Just g4,Just f) -> 
            playIO (InWindow "Pac-man" (620, 650) (340, 0)) 
                    black            -- Background color
                    10               -- Frames per second
                    beginstaat     -- Initial state
                    (view gen pac g1 g2 g3 g4 f)            -- View function
                    input            -- Event function
                    step             -- Step function
        _ ->
            putStrLn "Afbeelding kon niet worden geladen"