module Model where

type Grid = [[String]]
type Cell = (Int, Int)

data Dir = DUp | DDown | DLeft | DRight | DNone
  deriving (Show, Eq)

data PacPict = PacU | PacD | PacL | PacR
  deriving (Show, Eq)

data GhostName = Blinky | Pinky | Inky | Clyde
  deriving (Show, Eq)

data Player = Player
  {
  cell     :: Cell,
  next     :: Cell,
  movingP   :: Dir,
  wanted   :: Dir,
  progressP :: Float,
  hunting  :: Int,
  pict     :: PacPict
  } deriving (Show, Eq)

-- Places the player at the start of the game
initialPlayer :: Cell -> Player
initialPlayer c = Player
  { cell = c, next = c, movingP = DNone, wanted = DNone, progressP = 0, hunting = 0, pict = PacR }

data BaseGhost = BaseGhost
  {
  ghostCell :: Cell,
  nextCell :: Cell,
  moving :: Dir,
  progress :: Float,
  ghostSpeed :: Float,
  ghostName :: GhostName
  } deriving (Show, Eq)

newGhost :: GhostName -> Cell -> BaseGhost
newGhost n c = BaseGhost
  { ghostCell = c, nextCell = c, moving = DNone
  , progress = 0, ghostSpeed = 5, ghostName = n }

-- score, highscore en lives toegevoegd
data GameState = GameState
  { veld :: [[String]]
  , player :: Player
  , ghosts :: [BaseGhost]
  , score :: Int
  , highscore :: Int
  , lives :: Float
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
  , score = 0
  , highscore = 0
  , lives = 3
  }
