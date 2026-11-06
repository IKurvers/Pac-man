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

newGhost :: GhostName -> Cell -> BaseGhost
newGhost n c = BaseGhost
  { ghostCell = c, nextCell = c, moving = DNone
  , progress = 0, ghostSpeed = 5, ghostName = n, ghostMode = Normal, ghostHome = c, hunting = 0 }

-- score, highscore en lives toegevoegd
data GameState = GameState
  { veld :: [[String]]
  , player :: Player
  , ghosts :: [BaseGhost]
  , score :: Int
  , highscore :: Int
  , lives :: Float
  , paused :: Bool
  , died :: Bool
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
  , died      = read (allLines !! 43)
  }

hoekenStartLB :: Grid -> Int -> Int -> Cell
hoekenStartLB (("l":"-":xs):ys) x y = (x,y)
hoekenStartLB ([x1]:ys) x y = hoekenStartLB ys 0 (y+1)
hoekenStartLB ((x1:x2:xs):ys) x y = hoekenStartLB ((x2:xs):ys) (x+1) y
hoekenStartRB :: Grid -> Int -> Int -> Cell
hoekenStartRB (("-":"r":xs):ys) x y = ((x+1),y)
hoekenStartRB ([x1]:ys) x y = hoekenStartRB ys 0 (y+1)
hoekenStartRB ((x1:x2:xs):ys) x y = hoekenStartRB ((x2:xs):ys) (x+1) y 
hoekenStartLO :: Grid -> Int -> Int -> Cell
hoekenStartLO (("l":"_":xs):ys) x y = (x,y)
hoekenStartLO ([x1]:ys) x y = hoekenStartLO ys 0 (y+1)
hoekenStartLO ((x1:x2:xs):ys) x y = hoekenStartLO ((x2:xs):ys) (x+1) y
hoekenStartRO :: Grid -> Int -> Int -> Cell
hoekenStartRO (("_":"r":xs):ys) x y = ((x+1),y)
hoekenStartRO ([x1]:ys) x y = hoekenStartRO ys 0 (y+1) 
hoekenStartRO ((x1:x2:xs):ys) x y = hoekenStartRO ((x2:xs):ys) (x+1) y


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

startPos :: Grid -> GhostName -> Cell
startPos veld naam = 
  case naam of
    Blinky -> ((lox+1),(loy-1)) 
    Pinky -> ((lbx+2),(lby+1))
    Inky -> ((rox-2),(roy-1))
    Clyde -> ((rbx-1),(rby+1))
  where ((lbx,lby),(rbx,rby),(lox,loy),(rox,roy)) = (hoekenStartLB veld 0 0,hoekenStartRB veld 0 0,hoekenStartLO veld 0 0,hoekenStartRO veld 0 0)
