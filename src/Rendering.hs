module Rendering
  (renderGame
  ) where

import GameBoard

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)


-- | render dot that indicate the potential hiting point
renderDot :: Color    -- ^ dot color
          -> Position -- ^ dot position
          -> Radius   -- ^ dot radius
          -> Picture  -- ^ Picture of the dot
renderDot c (x, y) radius = translate x y $ color c $  circleSolid radius

-- | render state text
renderStateText :: Color  -- ^ Text color
            -> String     -- ^ Text
            -> Position   -- ^ Text position
            -> (Float, Float) -- ^ Text scale
            -> Picture    -- ^ Picture of the text
renderStateText col text (x, y) (sx, sy) = translate x y $ scale sx sy $ color col $ Text text

-- | render wall
renderWall :: Color       -- ^ Wall's color
       -> Width           -- ^ Wall's width
       -> Height          -- ^ Wall's height
       -> Position        -- ^ Wall's center
       -> Picture         -- ^ Wall's picture
renderWall col width height (x , y) =
      translate x y $ color col $ rectangleSolid width height

-- | render ball
renderBall :: Color       -- ^ Ball's color
       -> Radius          -- ^ Ball's radius
       -> (Float, Float)  -- ^ Ball's center
       -> Picture         -- ^ Picture of this ball
renderBall col radius (x, y) = translate x y  $ color col $ circleSolid radius

-- | render brick
renderBrick :: Brick    -- ^ the brick to render
        -> Picture  -- ^ brick picture
renderBrick b = translate x y $ color (brickCol b) $ rectangleSolid brickWidth brickHeight
      where
        (x, y) = brickLoc b

-- render paddle
renderPaddle :: Color -- ^ the paddle color
        -> Width      -- ^ paddle width
        -> Height     -- ^ paddle height
        -> Position   -- ^ paddle position
        -> Picture    -- ^ paddle picture
renderPaddle c w h (x, y) = translate x y $ color c $ rectangleSolid w h

-- | render the game
renderGame :: Game      -- ^ The game state to render
           -> Picture   -- ^ A picture of this game state

-- MainMenu state
renderGame game @ Game { gameState = MainMenu } = pictures
      [ renderStateText orange "Haskell" (-120, 100) (0.5, 0.5)
      , renderStateText orange "Breakout" (-150, 0) (0.5, 0.5)
      , renderStateText orange "Press ENTER to continu" (-200, -100) (0.25, 0.25)
      ]

-- GameOver state
renderGame game @ Game { gameState = GameOver } =
      renderStateText orange "Game Over" (-170, 0) (0.5, 0.5)

-- Paused state
renderGame game @ Game { gameState = Paused } =
      renderStateText orange "PAUSED" (-120, 0) (0.5, 0.5)

-- Playing state
renderGame game @ Game { gameState = Playing } = pictures
      [ renderBall (dark red) 10 (ballLoc game)
      , renderWall wallColor gameWidth wallWidth wallUpPos
--      , renderWall wallColor gameWidth wallWidth wallDownPos
      , renderWall wallColor wallWidth gameHeight wallLeftPos
      , renderWall wallColor wallWidth gameHeight wallRightPos
      , pictures . fmap renderBrick $ bricks game
      , renderPaddle paddleColor paddleWidth paddleHeight (paddleLoc $ paddle game)
      , renderDot white (ballDot game) 2
      ]
      where
        wallColor = blue
        brickColor = yellow
        paddleColor = cyan
