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
    , brickToRectangle
    , paddleToRectangle
    , initialState
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
  MainMenu | Playing | Paused | GameOver
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
foldBrick :: Position           -- ^ position (to build the new brick)
          -> (StdGen, [Brick])  -- ^ random generator + initial list of bricks
          -> (StdGen, [Brick])  -- ^ new gen (returned by the random operation)
                                --   and the new list of bricks (with the new one)
foldBrick pos (sg, bricks) =
      case isItem of
            True  -> (newSG',Brick yellow (Just i) pos:bricks)
            False -> (newSG',Brick yellow Nothing pos:bricks)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
        where (isItem :: Bool, newSG) = random sg
              (r, newSG') = randomR (1 :: Integer, 2) newSG
              i = case r of
                    1 -> PaddleExpander
                    2 -> PaddleMinifier

-- | make a list a bricks from a list of position
mkBricks :: [Position]
         -> [Brick]
mkBricks posLts = bricks
-- todo, need to call foldr with foldBrick function
      where (_, bricks) = foldr foldBrick (gen, []) posLts
            gen = mkStdGen 1234


-- | Create the first level
levelOne :: Level
levelOne = mkBricks brickPos
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
    , bricks = levelOne
    , paddle = Paddle { paddleLoc = (0,-(gameHeight / 2) + 50)
                      , paddleVel = (0,0)
                      , paddleWidth = 100
                      }
    , items = []
    }
