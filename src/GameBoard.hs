module GameBoard where

  winWidth, winHeight, offset:: Int
  winWidth = 800
  winHeight = 800
  offset = 100

  wallWidth :: Float
  wallWidth = 20

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

  -- * Game
  data Game = Game
    { ballLoc :: (Float, Float) -- ^ ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ ball (x, y) velocity
    } deriving Show

  -- ** initial state of the game
  initialState :: Game
  initialState = Game
    { ballLoc = (0, 0)
    , ballVel = (40, -140)
    }
