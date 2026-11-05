module Controller where

import Model

import Graphics.Gloss.Interface.IO.Game
import Model (Player, Dir, Grid, Cell)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Items

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate
  { player = updatedPlayer
  , ghosts =
      map (moveGhost (veld gstate) True (cell updatedPlayer) (movingP updatedPlayer) secs)
        (ghosts gstate)
  , veld = veld (eatenItem gstate (whichItem gstate))
  }
  where
    updatedPlayer = updatePlayer (veld gstate) secs (player gstate)

-- | Determine whether there's an item on the location
whichItem :: GameState -> Maybe ItemType
whichItem gstate | veld gstate !! y !! x == "o"    = Just SmallDot
                 | veld gstate !! y !! x == "0"    = Just BigDot
                 | otherwise                       = Nothing
                     where (x,y) = cell (player gstate)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate
  { player = handleInput e (player gstate) }

-- Constant speed
speed :: Float
speed = 6

-- Input handler that handles WASD and arrows, even with caps lock on
handleInput :: Event -> Player -> Player
handleInput (EventKey (Char k) Down _ _) p = case k of
  'w' -> p { wanted = DUp, pict = PacU }
  's' -> p { wanted = DDown, pict = PacD }
  'a' -> p { wanted = DLeft, pict = PacL }
  'd' -> p { wanted = DRight, pict = PacR }
  'W' -> p { wanted = DUp, pict = PacU }
  'S' -> p { wanted = DDown, pict = PacD }
  'A' -> p { wanted = DLeft, pict = PacL }
  'D' -> p { wanted = DRight, pict = PacR }
  _   -> p
handleInput (EventKey (SpecialKey k) Down _ _) p = case k of
  KeyUp    -> p { wanted = DUp, pict = PacU }
  KeyDown  -> p { wanted = DDown, pict = PacD }
  KeyLeft  -> p { wanted = DLeft, pict = PacL }
  KeyRight -> p { wanted = DRight, pict = PacR }
  _        -> p
handleInput _ p = p

-- Smooth positioning for visuals
smoothCell :: Player -> (Float, Float)
smoothCell p = (fromIntegral c + fromIntegral (nc - c) * t,
                fromIntegral r + fromIntegral (nr - r) * t)
  where
    (c, r)   = cell p
    (nc, nr) = next p
    t        = progressP p

-- Direction to vector for math
toVector :: Dir -> (Int, Int)
toVector DUp    = (0, -1)
toVector DDown  = (0, 1)
toVector DLeft  = (-1, 0)
toVector DRight = (1, 0)
toVector DNone  = (0, 0)

-- Easy way to get the opposite direction
opposite :: Dir -> Dir
opposite DUp    = DDown
opposite DDown  = DUp
opposite DLeft  = DRight
opposite DRight = DLeft
opposite DNone  = DNone

-- Moves you one cell in the direction, and from one side of the screen to the other if there's space
stepCell :: Grid -> Dir -> Cell -> Cell
stepCell grid d (c, r) | c == 0                       = (length (grid !! r) - 1 + dx, r + dy)
                       | c == length (grid !! r) -1   = (dx, r + dy)
                       | r == 0                       = (c + dx, length grid - 1 + dy)
                       | r == length grid -1          = (c + dx, dy)
                       | otherwise                    = (c + dx, r + dy)
  where (dx, dy) = toVector d

-- Wall Checker (True if wall)
isWall :: Grid -> Cell -> Bool
isWall grid (c, r)
  | r < 0 || r >= length grid = True
  | c < 0 || c >= length (grid !! r) = True
  -- toegevoegd: ook het startvak van de geestjes is een muur
  | otherwise = grid !! r !! c == "w" || grid !! r !! c == "n" || grid !! r !! c == "r" || grid !! r !! c == "l" || grid !! r !! c == "-" || grid !! r !! c == "_"

-- Main updator of the player functions. Handles movement, directions etc.
updatePlayer :: Grid -> Float -> Player -> Player
updatePlayer grid dt p
  | movingP p /= DNone && wanted p == opposite (movingP p) =
      p { cell     = next p
        , next     = cell p
        , movingP   = wanted p
        , progressP = 1 - progressP p }
  | movingP p == DNone = settle grid p
  | t >= 1 = settle grid (p { cell = next p, progressP = t - 1 })
  | otherwise = p { progressP = t }
  where
    t = progressP p + speed * min dt 0.05

-- Main helper function for movement
settle :: Grid -> Player -> Player
settle grid p = case pick of
  DNone -> p { movingP = DNone, next = cell p, progressP = 0 }
  d     -> p { movingP = d, next = stepCell grid d (cell p) }
  where
    open d = d /= DNone && not (isWall grid (stepCell grid d (cell p)))
    pick
      | open (wanted p) = wanted p
      | open (movingP p) = movingP p
      | otherwise       = DNone


-- Ghosts position as a tuple of floats
ghostPosition :: BaseGhost -> (Float, Float)
ghostPosition g = (fromIntegral c + fromIntegral (nc - c) * t,
                   fromIntegral r + fromIntegral (nr - r) * t)
  where
    (c, r) = ghostCell g
    (nc, nr) = nextCell g
    t = progress g

--Compiler does not like this, but if the grid is empty, there is no game anyway.
scatterTarget :: Grid -> GhostName -> Cell
scatterTarget grid n = case n of
  Blinky -> (w, -1)
  Pinky  -> (-1, -1)
  Inky   -> (w, h)
  Clyde  -> (-1, h)
  where
    h = length grid
    w = length (head grid)

-- Ai for the ghosts, like the original. Gives us the target position on the grid.
huntTarget :: Grid -> Cell -> Dir -> BaseGhost -> Cell
huntTarget grid playerCell playerDir g = case ghostName g of
  Blinky -> playerCell
  Pinky  -> (px + 4 * dx, py + 4 * dy)
  Inky   -> (fst ref + fst disVec, snd ref + snd disVec)
  Clyde
    | distTo (ghostCell g) playerCell > 8 * 8 -> playerCell
    | otherwise -> scatterTarget grid Clyde
  where
    (px, py) = playerCell
    (dx, dy) = toVector playerDir
    ref = (px + 2 * dx, py + 2 * dy)
    disVec = (fst ref - fst (ghostCell g), snd ref - snd (ghostCell g))

-- Determine the target, depending on whether the ghost is hunting or not.
ghostTarget :: Grid -> Bool -> Cell -> Dir -> BaseGhost -> Cell
ghostTarget grid isHunting playerCell playerDir g
  | isHunting = huntTarget grid playerCell playerDir g
  | otherwise = scatterTarget grid (ghostName g)

--Enum for direction.
directions :: [Dir]
directions = [DUp, DLeft, DDown, DRight]

-- Basic pythagorean formula
distTo :: Cell -> Cell -> Int
distTo (a, b) (c, d) = (a - c) ^ 2 + (b - d) ^ 2

-- Iplementation of BFS on the grid.
reachable :: Grid -> Cell -> [(Cell, Dir)]
reachable grid start = go (Set.singleton start) [(start, DNone)]
  where
    go _ [] = []
    go seen frontier = found ++ go seen' found
      where (found, seen') = expandAll seen frontier

-- Helper function, but I kept it inside so I could more easily use the variables. 
    expandAll seen [] = ([], seen)
    expandAll seen ((c, firstDir) : rest) = (found ++ more, seen2)
      where
        (found, seen1) = foldl visit ([], seen) directions
        (more, seen2)  = expandAll seen1 rest
        visit (acc, s) d
          | isWall grid n || Set.member n s = (acc, s)
          | otherwise =
              ( acc ++ [(n, if firstDir == DNone then d else firstDir)]
              , Set.insert n s )
          where n = stepCell grid d c

-- Moves you in the direction of target, to the cell that reduces the distance the most
firstStep :: Grid -> Cell -> Cell -> Dir
firstStep grid start target =
  snd (minimumBy (comparing (distTo target . fst))
                 ((start, DNone) : reachable grid start))

-- Sets the ghost in the direction of the target
settleGhost :: Grid -> Cell -> BaseGhost -> BaseGhost
settleGhost grid target g = case firstStep grid (ghostCell g) target of
  DNone -> g { moving = DNone, nextCell = ghostCell g, progress = 0 }
  d     -> g { moving = d, nextCell = stepCell grid d (ghostCell g) }

-- Updates the ghost, moving it towards the target or changing direction
updateGhost :: Grid -> Float -> Cell -> BaseGhost -> BaseGhost
updateGhost grid dt target g
  | moving g == DNone = settleGhost grid target g
  | t >= 1 = settleGhost grid target
               (g { ghostCell = nextCell g, progress = t - 1 })
  | otherwise = g { progress = t }
  where
    t = progress g + ghostSpeed g * min dt 0.05

-- Moving the ghost towards the target
moveGhost :: Grid -> Bool -> Cell -> Dir -> Float -> BaseGhost -> BaseGhost
moveGhost grid isHunting playerCell playerDir dt g =
  updateGhost grid dt (ghostTarget grid isHunting playerCell playerDir g) g

