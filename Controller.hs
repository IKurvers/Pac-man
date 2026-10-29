module Controller where

import Model

import BaseGhost (moveGhost)
import qualified Player as P
import Graphics.Gloss.Interface.IO.Game

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate
  { player = updatedPlayer
  , ghosts =
      map (moveGhost (veld gstate) True (P.cell updatedPlayer) (P.moving updatedPlayer) secs)
        (ghosts gstate)
  }
  where
    updatedPlayer = P.updatePlayer (veld gstate) secs (player gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate
  { player = P.handleInput e (player gstate) }