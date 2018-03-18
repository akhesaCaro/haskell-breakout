module GameBoard
    ( winHeight, winWidth, offset
    , gameWidth, gameHeight
    , wallWidth
    , addScore
    , brickWidth, brickHeight
    , ballRadius, speedRatio
    , wallUpPos, wallDownPos, wallLeftPos, wallRightPos
    , paddleWidth, paddleHeight, paddleStep
    , Position
    , Score
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
import Data.Bool

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
speedRatio = 1.025
paddleStep = 1
brickStepX = brickWidth + 10
brickStepY = brickHeight + 10

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
type Score = Integer
type Radius = Float
type Level = [Brick]
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
    { gameState :: GameState  -- ^ game state
    , gameScore :: Score      -- ^ game score
    , mouseEvent :: Bool      -- ^ if the system got a mouse event since last update
    , ballLoc :: Position     -- ^ ball (x, y) location.
    , ballVel :: Velocity     -- ^ ball (x, y) velocity
    , ballDot :: Position     -- ^ velocity indicator
    , bricks :: [Brick]       -- ^ bricks list
    , paddle :: Paddle        -- ^ paddle
    } deriving Show

-- | Create the first level
levelOne :: Level
levelOne = map (flip Brick col) brickPos
      where
        grid = (,) <$> [-3..3] <*> [1..6]
        brickPos = map (\(x, y) -> (x*brickStepX, y*brickStepY)) grid
        col = yellow

-- | Add brick points to score.
addScore :: Score  -- ^ current score
         -> Score  -- ^ updated score
addScore = (+10)

-- | initial game state
initialState :: Game
initialState = Game
    { gameState = MainMenu
    , gameScore = 0
    , mouseEvent = False
    , ballLoc = (0, -200)
    , ballVel = (50, 150)
    , ballDot = (0, 0)
    , bricks = levelOne
    , paddle = Paddle { paddleLoc = (0,-(gameHeight / 2) + 50)
                      , paddleVel = (0,0)
                      }
    }
