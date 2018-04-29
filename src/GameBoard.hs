{-# LANGUAGE ScopedTypeVariables #-}

module GameBoard
    ( winHeight, winWidth, offset
    , gameWidth, gameHeight
    , wallWidth
    , addScore
    , brickWidth, brickHeight
    , ballRadius, speedRatio
    , wallUpPos, wallDownPos, wallLeftPos, wallRightPos
    , paddleHeight
    , itemWidth, itemHeight, itemVel
    , Position
    , Score
    , Radius
    , Velocity
    , Width
    , Height
    , Rectangle
    , ItemType (..)
    , Brick(..)
    , Game (..)
    , Paddle (..)
    , GameState (..)
    , Item (..)
    , GameLevel
    , brickToRectangle
    , paddleToRectangle
    , initialState
    , newLevelState
    ) where

-- I want to use my own Vector.
import Graphics.Gloss  hiding (Vector)

import System.Random

-- | position of the window on the screen
offset:: Int
offset = 100

-- | Window width and height
winHeight, winWidth :: Int
winWidth = floor (gameWidth + wallWidth * 2)
winHeight = floor (gameHeight + wallWidth * 2)

-- | all the widths : brick, game, paddle
-- wallWidth = thickness
gameWidth, wallWidth, brickWidth, itemWidth :: Width
gameWidth = 800
wallWidth = 10
brickWidth = 100
itemWidth  = 15

-- | speed ration (when the ball hits a brick)
speedRatio :: Float
speedRatio = 1.025
brickStepX = brickWidth + 10
brickStepY = brickHeight + 10

-- | all the heights : brick, game, paddle
brickHeight, gameHeight, paddleHeight, itemHeight :: Height
brickHeight = 40
gameHeight = 800
paddleHeight = 20
itemHeight = 15

-- | Radius of the ball
ballRadius :: Radius
ballRadius = 10

-- | All the wall positions
wallUpPos, wallDownPos, wallLeftPos, wallRightPos :: Position
wallUpPos    = (0, gameHeight / 2)      -- ^ top wall position
wallDownPos  = (0,-(gameHeight/ 2))     -- ^ botom wall position
wallLeftPos  = (-(gameWidth / 2) , 0)   -- ^ left wall position
wallRightPos = (gameWidth / 2, 0)       -- ^ right wall position

itemVel :: Velocity
itemVel = (0, -5)

-- | aliases
type Score = Integer
type Radius = Float
type Level = [Brick]
type GameLevel = Integer
type Velocity = (Float, Float)
type Position = (Float, Float)
type Width = Float
type Height = Float
type Rectangle = (Position, Width, Height)


-- |  brick item type (bonus or malus)
data ItemType = PaddleExpander | PaddleMinifier
  deriving Show

-- | The game state
data GameState =
  MainMenu | Playing | Paused | GameOver | Win | NextLevel
  deriving Show

-- | Brick
data Brick = Brick
    { brickCol  :: Color            -- ^ brick color
    , brickItem :: Maybe ItemType   -- ^ item type hidden in the brick
    , brickLoc  :: Position         -- ^ brick (x, y) location
    } deriving Show

-- | Paddle
data Paddle = Paddle
    { paddleLoc   :: Position   -- ^ paddle (x, y) location
    , paddleVel   :: Velocity   -- ^ paddle velocity
    , paddleWidth :: Width      -- ^ paddle width
    } deriving Show

-- | Item
data Item = Item
    { itemType :: ItemType  -- ^ Item type
    , itemPos  :: Position  -- ^ Item position
    } deriving Show

-- | Game
data Game = Game
    { gameState :: GameState  -- ^ game state
    , gameScore :: Score      -- ^ game score
    , mouseEvent :: Bool      -- ^ if the system got a mouse event since last update
    , ballLoc :: Position     -- ^ ball (x, y) location.
    , ballVel :: Velocity     -- ^ ball (x, y) velocity
    , ballDots :: [Position]  -- ^ dot used for the collision 3 vectors
    , bricks :: [Brick]       -- ^ bricks list
    , paddle :: Paddle        -- ^ paddle
    , items :: [Item]         -- ^ items falling
    , level :: GameLevel      -- ^ level number
    } deriving Show

-- | Transform a brick to a rectangle
brickToRectangle :: Brick     -- ^ brick to transform
                 -> Rectangle -- ^ brick transformed to a rectangle
brickToRectangle b = (brickLoc b, brickWidth, brickHeight)

-- | Transform a paddle to a rectangle
paddleToRectangle :: Paddle     -- ^ paddle to transform
                  -> Rectangle  -- ^ paddle transformed to a rectangle
paddleToRectangle p = (paddleLoc p, paddleWidth p, paddleHeight)


-- |  Foldbrick function to use in the foldr
--    From a tuple of a ranomGen and list of bricks and a position,
--    it return a new generator and a List with a new brick made with the position
foldBrick :: GameLevel                -- ^ level number (probability to have a brick)
          -> Position                   -- ^ position (to build the new brick)
          -> (StdGen, StdGen, [Brick])  -- ^ random brick + + random item + initial list of bricks
          -> (StdGen, StdGen, [Brick])  -- ^ new gen (returned by the random operation)
                                        --   and the new list of bricks (with the new one)
foldBrick level pos (sgItem, sgBrick, bricks) =
      case isBrick of
            True -> case isItem of
                          True  -> (newSGItem', newSGBrick, Brick yellow (Just itemType) pos:bricks)
                          False -> (newSGItem', newSGBrick, Brick yellow Nothing pos:bricks)
            False -> (newSGItem', newSGBrick, bricks)
      where (isItem :: Bool, newSGItem) = random sgItem
            (rItem, newSGItem') = randomR (1 :: Integer, 2) newSGItem
            itemType = case rItem of
                  1 -> PaddleExpander
                  2 -> PaddleMinifier
            -- Is there a brick or not
            (rBrick, newSGBrick) = randomR (1 :: Integer, 10) sgBrick
            isBrick = if rBrick <= level
                      then True
                      else False

-- | make a list a bricks from a list of position
mkBricks :: [Position]    -- ^ Position where to draw bricks
         -> GameLevel     -- ^ game level (probability to render all bricks)
         -> [Brick]
mkBricks posLts level  = bricks
      where (_, _, bricks) = foldr foldBrickWithLevel (genItem, genBrick, []) posLts
            foldBrickWithLevel = foldBrick level
            genItem   = mkStdGen 1234
            genBrick  = mkStdGen 1235


-- | Create the first level
mkLevel :: GameLevel
        -> Level
mkLevel gl = mkBricks brickPos gl
      where
        grid = (,) <$> [-3..3] <*> [1..6]
        brickPos = map (\(x, y) -> (x*brickStepX, y*brickStepY)) grid


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
    , ballDots = [(0, 0)]
    , bricks = mkLevel 1
    , paddle = Paddle { paddleLoc = (0,-(gameHeight / 2) + 50)
                      , paddleVel = (0,0)
                      , paddleWidth = 100
                      }
    , items = []
    , level = 1
    }

-- | new Level
newLevelState :: GameLevel -- ^ game level
              -> Score     -- ^ game score
              -> Game      -- ^ new game state
newLevelState l s = Game
    { gameState = NextLevel
    , gameScore = s
    , mouseEvent = False
    , ballLoc = (0, -200)
    , ballVel = (50, 150)
    , ballDots = [(0, 0)]
    , bricks = mkLevel l
    , paddle = Paddle { paddleLoc = (0,-(gameHeight / 2) + 50)
                      , paddleVel = (0,0)
                      , paddleWidth = 100
                      }
    , items = []
    , level = l
    }
