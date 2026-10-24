module Player where

import Graphics.Gloss.Interface.IO.Game

type Grid = [[Int]]
type Cell = (Int, Int)

data Dir = DUp | DDown | DLeft | DRight | DNone
  deriving (Show, Eq)

data Player = Player
  {
  cell     :: Cell,
  next     :: Cell,
  moving   :: Dir,
  wanted   :: Dir,
  progress :: Float,
  hunting  :: Int
  } deriving (Show, Eq)

speed :: Float
speed = 6

wallValue :: Int
wallValue = 1

initialPlayer :: Cell -> Player
initialPlayer c = Player
  { cell = c, next = c, moving = DNone, wanted = DNone, progress = 0, hunting = 0 }

handleInput :: Event -> Player -> Player
handleInput (EventKey (Char k) Down _ _) p = case k of
  'w' -> p { wanted = DUp }
  's' -> p { wanted = DDown }
  'a' -> p { wanted = DLeft }
  'd' -> p { wanted = DRight }
  'W' -> p { wanted = DUp }
  'S' -> p { wanted = DDown }
  'A' -> p { wanted = DLeft }
  'D' -> p { wanted = DRight }
  _   -> p
handleInput (EventKey (SpecialKey k) Down _ _) p = case k of
  KeyUp    -> p { wanted = DUp }
  KeyDown  -> p { wanted = DDown }
  KeyLeft  -> p { wanted = DLeft }
  KeyRight -> p { wanted = DRight }
  _        -> p
handleInput _ p = p

smoothCell :: Player -> (Float, Float)
smoothCell p = (fromIntegral c + fromIntegral (nc - c) * t,
                fromIntegral r + fromIntegral (nr - r) * t)
  where
    (c, r)   = cell p
    (nc, nr) = next p
    t        = progress p

toVector :: Dir -> (Int, Int)
toVector DUp    = (0, -1)
toVector DDown  = (0, 1)
toVector DLeft  = (-1, 0)
toVector DRight = (1, 0)
toVector DNone  = (0, 0)

opposite :: Dir -> Dir
opposite DUp    = DDown
opposite DDown  = DUp
opposite DLeft  = DRight
opposite DRight = DLeft
opposite DNone  = DNone

stepCell :: Dir -> Cell -> Cell
stepCell d (c, r) = (c + dx, r + dy)
  where (dx, dy) = toVector d

isWall :: Grid -> Cell -> Bool
isWall grid (c, r)
  | r < 0 || r >= length grid = True
  | c < 0 || c >= length (grid !! r) = True
  | otherwise = grid !! r !! c == wallValue

updatePlayer :: Grid -> Float -> Player -> Player
updatePlayer grid dt p
  | moving p /= DNone && wanted p == opposite (moving p) =
      p { cell     = next p
        , next     = cell p
        , moving   = wanted p
        , progress = 1 - progress p }
  | moving p == DNone = settle grid p
  | t >= 1 = settle grid (p { cell = next p, progress = t - 1 })
  | otherwise = p { progress = t }
  where
    t = progress p + speed * min dt 0.05


settle :: Grid -> Player -> Player
settle grid p = case pick of
  DNone -> p { moving = DNone, next = cell p, progress = 0 }
  d     -> p { moving = d, next = stepCell d (cell p) }
  where
    open d = d /= DNone && not (isWall grid (stepCell d (cell p)))
    pick
      | open (wanted p) = wanted p
      | open (moving p) = moving p
      | otherwise       = DNone