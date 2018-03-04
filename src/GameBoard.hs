module GameBoard
  ( winHeight, winWidth, offset
  , gameWidth, gameHeight
  , wallWidth
  , brickWidth, brickHeight
  , ballRadius
  , wallUpPos, wallDownPos, wallLeftPos, wallRightPos
  , Position
  , Radius
  , Width
  , Height
  , Brick(..)
  , Game (..)
  , initialState
  ) where

  import Graphics.Gloss

  -- | position of the window on the screen
  offset:: Int
  offset = 100

  -- | Window width and height
  winHeight, winWidth :: Int
  winWidth = floor (gameWidth + wallWidth * 2)
  winHeight = floor (gameHeight + wallWidth * 2)

  -- | all the widths : brick, game
  -- wallWidth = thickness
  gameWidth, wallWidth, brickWidth, paddleWidth :: Width
  gameWidth = 800
  wallWidth = 10
  brickWidth = 100
  paddleWidth = 100

  -- | speed ration (when the ball hits a brick)
  speedRatio :: Float
  speedRatio = 1.5

<<<<<<< ef207458608785b7998e8f3d8c39008da8a43327
  -- | all the widths : brick, wall, game
  gameHeight, brickHeight, paddleHeight :: Height
  gameHeight = 800
  brickHeight = 40
  paddleHeight = 10
=======
  brickWidth, brickHeight, speedRatio, paddleStep :: Float
  brickWidth = 100
  brickHeight = 40
  speedRatio = 1.5
  paddleStep = 5
>>>>>>> Better collision with paddle and walls limit

  -- | Radius of the ball
  ballRadius :: Radius
  ballRadius = 10

  -- | All the wall positions
  wallUpPos, wallDownPos, wallLeftPos, wallRightPos :: Position
  wallUpPos   = (0, gameHeight / 2)      -- ^ top wall position
  wallDownPos = (0,-(gameHeight/ 2))   -- ^ botom wall position
  wallLeftPos = (gameWidth / 2 , 0)      -- ^ left wall position
  wallRightPos = (-(gameWidth / 2), 0)  -- ^ right wall position


  -- | aliases
  type Radius = Float
  type Velocity = (Float, Float)
  type Position = (Float, Float)
  type Width = Float
  type Height = Float

  -- | Brick
  data Brick = Brick
        { brickLoc :: Position -- ^ brick (x, y) location
        , brickCol :: Color          -- ^ brick color
        } deriving Show

  -- | Paddle
  data Paddle = Paddle
    { paddleLoc :: Position   -- ^ paddle (x, y) location
    , paddleVel :: Velocity   -- ^ paddle velocity
    } deriving Show

  -- | Game
  data Game = Game
    { ballLoc :: Position     -- ^ ball (x, y) location.
    , ballVel :: Velocity     -- ^ ball (x, y) velocity
    , bricks :: [Brick]       -- ^ bricks list
    , paddle :: Paddle         -- ^ paddle
    } deriving Show

  -- | initial state of the game
  initialState :: Game
  initialState = Game
    { ballLoc = (0, 0)
    , ballVel = (40, -140)
    , bricks = [ Brick {brickLoc = (-300, 300), brickCol = yellow}
               , Brick {brickLoc = (0, 0), brickCol = blue}
               , Brick {brickLoc = (50, 50), brickCol = magenta}
               , Brick {brickLoc = (0, 300), brickCol = magenta}
               , Brick {brickLoc = (-200, 100), brickCol = yellow}
               , Brick {brickLoc = (-250, -250), brickCol = yellow}
               ]
    , paddle = Paddle { paddleLoc = (0,-(gameHeight / 2) + 50)
                      , paddleVel = (0,0)
                      }
    }
