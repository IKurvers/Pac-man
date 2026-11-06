module Main where

import Controller
import Model 
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Random

main :: IO ()
main = do
    textinput <- readFile "SaveState.txt"
    let beginveld = map words (lines textinput)
    let beginstaat = initialState (map words (lines textinput))

    gen <- getStdGen

    pics <- loadPictures

    geest1 <- loadJuicyPNG "geest1.png"
    geest2 <- loadJuicyPNG "geest2.png"
    geest3 <- loadJuicyPNG "geest3.png"
    geest4 <- loadJuicyPNG "geest4.png"
    oogjes <- loadJuicyPNG "oogjes.png"
    doodGeest <- loadJuicyPNG "doodGeest.png"
    kers <- loadJuicyPNG "cherry.png"
    hart <- loadJuicyPNG "hart.png"

    case (pics, geest1, geest2, geest3, geest4, kers, hart, oogjes, doodGeest) of
        (Just ps, Just g1, Just g2, Just g3, Just g4, Just k, Just h, Just o, Just dood) ->
            playIO (InWindow "Pac-man" (620, 685) (340, 0))
                    black
                    10
                    beginstaat
                    (view gen ps g1 g2 g3 g4 k h o dood)
                    input
                    step

        _ -> putStrLn "Afbeelding kon niet worden geladen"


loadPictures :: IO (Maybe Pict)
loadPictures = do
    up    <- loadJuicyPNG "pacmanU.png"
    down  <- loadJuicyPNG "pacmanD.png"
    left  <- loadJuicyPNG "pacmanL.png"
    right <- loadJuicyPNG "pacmanR.png"

    case (up, down, left, right) of
        (Just u, Just d, Just l, Just r) ->
            return (Just (Pict u d l r))
        _ -> return Nothing