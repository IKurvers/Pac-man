module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy

main :: IO ()
main = do
    tekstinput <- readFile "veld_pac-man.txt"
    let beginveld = map words (lines tekstinput)
    let beginstaat = initialState {veld = beginveld}

    pacmanZelf <- loadJuicyPNG "pacman-zelf.png"
    case pacmanZelf of
        Just plaatje -> do
            playIO (InWindow "Pac-man" (620, 650) (340, 0)) 
                    black            -- Background color
                    10               -- Frames per second
                    beginstaat     -- Initial state
                    (view plaatje)            -- View function
                    input            -- Event function
                    step             -- Step function
        Nothing ->
            putStrLn "Afbeelding kon niet worden geladen"