module Model where

import BaseGhost (BaseGhost, GhostName (..), newGhost)
import Player (Player, initialPlayer)

data GameState = GameState
  { veld :: [[String]]
  , player :: Player
  , ghosts :: [BaseGhost]
  }

initialState :: [[String]] -> GameState
initialState grid = GameState
  { veld = grid
  , player = initialPlayer (13, 21)
  , ghosts =
      [ newGhost Blinky (13, 11)
      , newGhost Pinky (14, 11)
      , newGhost Inky (15, 11)
      , newGhost Clyde (16, 11)
      ]
  }
