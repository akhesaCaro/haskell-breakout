module GameBoard where

  import Graphics.Gloss

  offset:: Int
  offset = 100

  winHeight, winWidth :: Int
  winWidth = floor (gameWidth + wallWidth * 2)
  winHeight = floor (gameHeight + wallWidth * 2)

  gameWidth, wallWidth :: Width
  gameWidth = 800
  wallWidth = 10

  gameHeight :: Height
  gameHeight = 800

  brickWidth, brickHeight :: Float
  brickWidth = 100
  brickHeight = 40

  ballRadius :: Radius
  ballRadius = 10

  wallUpPos, wallDownPos, wallLeftPos, wallRightPos :: (Float , Float)
  wallUpPos   = (0, gameHeight / 2)      -- ^ top wall position
  wallDownPos = (0,-(gameHeight/ 2))   -- ^ botom wall position
  wallLeftPos = (gameWidth / 2 , 0)      -- ^ left wall position
  wallRightPos = (-(gameWidth / 2), 0)  -- ^ right wall position

  -- * aliases
  type Radius = Float
  type Position = (Float, Float)
  type Width = Float
  type Height = Float

  -- *  Brick
  data Brick = Brick
        { brickLoc :: (Float, Float) -- ^ brick (x, y) location
        , brickCol :: Color          -- ^ brick color
        } deriving Show

  -- * Game
  data Game = Game
    { ballLoc :: (Float, Float) -- ^ ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ ball (x, y) velocity
    , bricks :: [Brick]         -- ^ bricks list
    } deriving Show

  -- ** initial state of the game
  initialState :: Game
  initialState = Game
    { ballLoc = (0, 0)
    , ballVel = (40, -140)
    , bricks = [ Brick {brickLoc = (-300, 300), brickCol = yellow}
               , Brick {brickLoc = (0, 0), brickCol = blue}
               , Brick {brickLoc = (50, 50), brickCol = magenta}
               , Brick {brickLoc = (0, 300), brickCol = white}
               , Brick {brickLoc = (-200, 100), brickCol = yellow}
               , Brick {brickLoc = (-250, -250), brickCol = cyan}
               ]
    }
