module GameBoard where

  import Graphics.Gloss

  winWidth, winHeight, offset:: Int
  winWidth = 800
  winHeight = 800
  offset = 100

  wallWidth, brickWidth, brickHeight :: Float
  wallWidth = 20
  brickWidth = 200
  brickHeight = 40

  ballRadius :: Radius
  ballRadius = 10

  wallUpPos, wallDownPos, wallLeftPos, wallRightPos :: (Float , Float)
  wallUpPos   = (0,fromIntegral winHeight / 2)      -- ^ top wall position
  wallDownPos = (0,-(fromIntegral winHeight / 2))   -- ^ botom wall position
  wallLeftPos = (fromIntegral winWidth / 2, 0)      -- ^ left wall position
  wallRightPos = (-(fromIntegral winWidth / 2), 0)  -- ^ right wall position
                         -- ^ ball velocity

  type Radius = Float
  type Position = (Float, Float)

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
    , bricks = [Brick {brickLoc = (0, 0), brickCol = yellow}]
    }
