module GameBoard
    ( winHeight, winWidth, offset
    , gameWidth, gameHeight
    , wallWidth
    , brickWidth, brickHeight
    , ballRadius, speedRatio
    , wallUpPos, wallDownPos, wallLeftPos, wallRightPos
    , paddleWidth, paddleHeight, paddleStep
    , Position
    , Radius
    , Velocity
    , Width
    , Height
    , Rectangle
    , Brick(..)
    , Game (..)
    , Paddle (..)
    , GameState (..)
    , initialState
    ) where

-- I want to use my own Vector.
import Graphics.Gloss  hiding (Vector)

-- | position of the window on the screen
offset:: Int
offset = 100

-- | Window width and height
winHeight, winWidth :: Int
winWidth = floor (gameWidth + wallWidth * 2)
winHeight = floor (gameHeight + wallWidth * 2)

-- | all the widths : brick, game, paddle
-- wallWidth = thickness
gameWidth, wallWidth, brickWidth, paddleWidth :: Width
gameWidth = 800
wallWidth = 10
brickWidth = 100
paddleWidth = 100

-- | speed ration (when the ball hits a brick)
speedRatio, paddleStep :: Float
speedRatio = 1.25
paddleStep = 5

-- | all the heights : brick, game, paddle
brickHeight, gameHeight, paddleHeight :: Height
brickHeight = 40
gameHeight = 800
paddleHeight = 20

-- | Radius of the ball
ballRadius :: Radius
ballRadius = 10

-- | All the wall positions
wallUpPos, wallDownPos, wallLeftPos, wallRightPos :: Position
wallUpPos    = (0, gameHeight / 2)     -- ^ top wall position
wallDownPos  = (0,-(gameHeight/ 2))    -- ^ botom wall position
wallLeftPos  = (-(gameWidth / 2) , 0)     -- ^ left wall position
wallRightPos = (gameWidth / 2, 0)   -- ^ right wall position


-- | aliases
type Radius = Float
type Velocity = (Float, Float)
type Position = (Float, Float)
type Width = Float
type Height = Float
type Rectangle = (Position, Width, Height)

-- | The game state
data GameState =
  MainMenu | Playing | Paused | GameOver
  deriving Show

-- | Brick
data Brick = Brick
    { brickLoc :: Position  -- ^ brick (x, y) location
    , brickCol :: Color     -- ^ brick color
    } deriving Show

-- | Paddle
data Paddle = Paddle
    { paddleLoc :: Position   -- ^ paddle (x, y) location
    , paddleVel :: Velocity   -- ^ paddle velocity
    } deriving Show

-- | Game
data Game = Game
    { gameState :: GameState
    , ballLoc :: Position   -- ^ ball (x, y) location.
    , ballVel :: Velocity   -- ^ ball (x, y) velocity
    , ballDot :: Position   -- ^ velocity indicator
    , bricks :: [Brick]     -- ^ bricks list
    , paddle :: Paddle      -- ^ paddle
    } deriving Show

-- | initial state of the game
initialState :: Game
initialState = Game
    { gameState = MainMenu
    , ballLoc = (0, -200)
    , ballVel = (50, -150)
    , ballDot = (0, 0)
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
