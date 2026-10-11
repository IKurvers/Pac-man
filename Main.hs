module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    tekstinput <- readFile "veld_pac-man.txt"
    let beginveld = map words (lines tekstinput)
    let beginstaat = initialState {veld = beginveld}

    playIO (InWindow "Pac-man" (500, 500) (400, 100)) 
              black            -- Background color
              10               -- Frames per second
              beginstaat     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
