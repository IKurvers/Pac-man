module BaseGhost where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

import Player (Grid, Cell, Dir (..), isWall, stepCell, toVector)

data GhostName = Blinky | Pinky | Inky | Clyde
  deriving (Show, Eq)

data BaseGhost = BaseGhost
  {
  ghostCell :: Cell,
  nextCell :: Cell,
  moving :: Dir,
  progress :: Float,
  speed :: Float,
  ghostName :: GhostName
  } deriving (Show, Eq)

newGhost :: GhostName -> Cell -> BaseGhost
newGhost n c = BaseGhost
  { ghostCell = c, nextCell = c, moving = DNone
  , progress = 0, speed = 5, ghostName = n }


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
          where n = stepCell d c

-- Moves you in the direction of target, to the cell that reduces the distance the most
firstStep :: Grid -> Cell -> Cell -> Dir
firstStep grid start target =
  snd (minimumBy (comparing (distTo target . fst))
                 ((start, DNone) : reachable grid start))

-- Sets the ghost in the direction of the target
settleGhost :: Grid -> Cell -> BaseGhost -> BaseGhost
settleGhost grid target g = case firstStep grid (ghostCell g) target of
  DNone -> g { moving = DNone, nextCell = ghostCell g, progress = 0 }
  d     -> g { moving = d, nextCell = stepCell d (ghostCell g) }

-- Updates the ghost, moving it towards the target or changing direction
updateGhost :: Grid -> Float -> Cell -> BaseGhost -> BaseGhost
updateGhost grid dt target g
  | moving g == DNone = settleGhost grid target g
  | t >= 1 = settleGhost grid target
               (g { ghostCell = nextCell g, progress = t - 1 })
  | otherwise = g { progress = t }
  where
    t = progress g + speed g * min dt 0.05

-- Moving the ghost towards the target
moveGhost :: Grid -> Bool -> Cell -> Dir -> Float -> BaseGhost -> BaseGhost
moveGhost grid isHunting playerCell playerDir dt g =
  updateGhost grid dt (ghostTarget grid isHunting playerCell playerDir g) g

