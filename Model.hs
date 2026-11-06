module Model where

type Grid = [[String]]
type Cell = (Int, Int)

data Dir = DUp | DDown | DLeft | DRight | DNone
  deriving (Show, Eq, Read)

data PacPict = PacU | PacD | PacL | PacR
  deriving (Show, Eq, Read)

data GhostName = Blinky | Pinky | Inky | Clyde
  deriving (Show, Eq, Read)

data GhostMode = Normal | Eyes
  deriving (Show, Eq, Read)

data Player = Player
  {
  cell     :: Cell,
  next     :: Cell,
  movingP   :: Dir,
  wanted   :: Dir,
  progressP :: Float,
  pict     :: PacPict
  } deriving (Show, Eq)

-- Places the player at the start of the game
initialPlayer :: Cell -> Player
initialPlayer c = Player
  { cell = c, next = c, movingP = DNone, wanted = DNone, progressP = 0, pict = PacR }

data BaseGhost = BaseGhost
  {
  ghostCell :: Cell,
  nextCell :: Cell,
  moving :: Dir,
  progress :: Float,
  ghostSpeed :: Float,
  ghostName :: GhostName,
  ghostMode :: GhostMode,
  ghostHome :: Cell,
  hunting :: Float
  } deriving (Show, Eq)
  
-- Places the ghost at the start of the game
newGhost :: GhostName -> Cell -> BaseGhost
newGhost n c = BaseGhost
  { ghostCell = c, nextCell = c, moving = DNone
  , progress = 0, ghostSpeed = 5, ghostName = n, ghostMode = Normal, ghostHome = c, hunting = 0 }

data GameState = GameState
  { veld :: [[String]]
  , player :: Player
  , ghosts :: [BaseGhost]
  , score :: Int
  , highscore :: Int
  , lives :: Float
  , paused :: Bool
  }

-- | Initializes the game state from a list of strings read from a file
initialState :: [String] -> GameState
initialState allLines = GameState
  { veld      = map words (take 29 allLines)
  , score     = read (allLines !! 29)
  , highscore = read (allLines !! 30)
  , lives     = read (allLines !! 31)
  , paused    = read (allLines !! 32)
  , player    = readPlayer (take 6 (drop 33 allLines))
  , ghosts    = map (readGhost . words) (take 4 (drop 39 allLines))
  }



-- Helper function to read the ghost information
readGhost :: [String] -> BaseGhost
readGhost [cLine, nLine, movLine, progLine, speedLine, nameLine, huntLine, modeLine] =
    BaseGhost {
        ghostCell  = read cLine,
        nextCell   = read nLine,
        moving     = read movLine,
        progress   = read progLine,
        ghostSpeed = read speedLine,
        ghostName  = read nameLine,
        hunting    = read huntLine,
        ghostMode  = read modeLine,
        ghostHome  = read cLine
    }
readGhost _ = error "Invalid ghost save data format"

-- Helper function to read the player information
readPlayer :: [String] -> Player
readPlayer [cLine, nLine, movLine, wanLine, progLine, picLine] =
    Player {
        cell      = read cLine,
        next      = read nLine,
        movingP   = read movLine,
        wanted    = read wanLine,
        progressP = read progLine,
        pict      = read picLine
    }
readPlayer _ = error "Invalid player save data format"
