module Model where

type Grid = [[String]]
type Cell = (Int, Int)

data Dir = DUp | DDown | DLeft | DRight | DNone
  deriving (Show, Eq, Read)

data PacPict = PacU | PacD | PacL | PacR
  deriving (Show, Eq, Read)

data GhostName = Blinky | Pinky | Inky | Clyde
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
  hunting :: Float
  } deriving (Show, Eq)

newGhost :: GhostName -> Cell -> BaseGhost
newGhost n c = BaseGhost
  { ghostCell = c, nextCell = c, moving = DNone
  , progress = 0, ghostSpeed = 5, ghostName = n, hunting = 0 }

-- score, highscore en lives toegevoegd
data GameState = GameState
  { veld :: [[String]]
  , player :: Player
  , ghosts :: [BaseGhost]
  , score :: Int
  , highscore :: Int
  , lives :: Float
  , paused :: Bool
  }

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




readGhost :: [String] -> BaseGhost
readGhost [cLine, nLine, movLine, progLine, speedLine, nameLine, huntLine] =
    BaseGhost {
        ghostCell  = read cLine,
        nextCell   = read nLine,
        moving     = read movLine,
        progress   = read progLine,
        ghostSpeed = read speedLine,
        ghostName  = read nameLine,
        hunting    = read huntLine
    }
readGhost _ = error "Invalid ghost save data format"

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
