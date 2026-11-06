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
    gen <- getStdGen
    let beginveld = map words (lines textinput)
    let (randomVeld, gen') = randomizeGrid gen beginveld
    let beginstaat = initialState (lines textinput)
    setStdGen gen'
    pics <- loadPictures

    geest1 <- loadJuicyPNG "geest1.png"
    geest2 <- loadJuicyPNG "geest2.png"
    geest3 <- loadJuicyPNG "geest3.png"
    geest4 <- loadJuicyPNG "geest4.png"
    eyes <- loadJuicyPNG "oogjes.png"
    doodGeest <- loadJuicyPNG "doodGeest.png"
    cherry <- loadJuicyPNG "cherry.png"
    strawberry <- loadJuicyPNG "aardbei.png"
    apple <- loadJuicyPNG "appel.png"
    orange <- loadJuicyPNG "orange.png"
    melon <- loadJuicyPNG "melon.png"
    hart <- loadJuicyPNG "hart.png"

    case (pics, geest1, geest2, geest3, geest4, cherry, strawberry, apple, orange, melon, hart, eyes, doodGeest) of
        (Just ps, Just g1, Just g2, Just g3, Just g4, Just c, Just s, Just a, Just r, Just m, Just h, Just e, Just dood) ->
            playIO (InWindow "Pac-man" (620, 685) (340, 0))
                    black
                    10
                    beginstaat
                    (view ps g1 g2 g3 g4 c s a r m h e dood)
                    input
                    step

        _ -> putStrLn "Afbeelding kon niet worden geladen"


loadPictures :: IO (Maybe Pict)
loadPictures = do
    up    <- loadJuicyPNG "pacmanU.png"
    down  <- loadJuicyPNG "pacmanD.png"
    left  <- loadJuicyPNG "pacmanL.png"
    right <- loadJuicyPNG "pacmanR.png"
    upHalf    <- loadJuicyPNG "pacmanUhalf.png"
    downHalf  <- loadJuicyPNG "pacmanDhalf.png"
    leftHalf  <- loadJuicyPNG "pacmanLhalf.png"
    rightHalf <- loadJuicyPNG "pacmanRhalf.png"
    heel <- loadJuicyPNG "pacmanheel.png"


    case (up, down, left, right, upHalf, downHalf, leftHalf, rightHalf, heel) of
        (Just u, Just d, Just l, Just r, Just uh, Just dh, Just lh, Just rh, Just h) ->
            return (Just (Pict u d l r uh dh lh rh h))
        _ -> return Nothing