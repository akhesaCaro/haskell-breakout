module Rendering
  (renderGameIO
  , Library (..)
  ) where

import GameBoard

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)

-- | Images library
data Library = Library
      { brickImg :: Picture
      }

-- | Render score
renderScore :: Score    -- ^ score to render
            -> Picture  -- ^ picture of the score
renderScore s = translate (-10) ((gameHeight / 2) - 50)
              $ scale 0.25 0.25
              $ color blue
              $ Text (show s)

-- | render dot that indicate the potential hiting point
renderDot :: Color    -- ^ dot color
          -> Radius   -- ^ dot radius
          -> Position -- ^ dot position
          -> Picture  -- ^ Picture of the dot
renderDot c radius (x, y) = translate x y
                          $ color c
                          $ circleSolid radius

-- | render state text
renderStateText :: Color      -- ^ Text color
            -> String         -- ^ Text
            -> Position       -- ^ Text position
            -> (Float, Float) -- ^ Scaling factors along X and Y dimensions.
            -> Picture        -- ^ Picture of the text
renderStateText col text (x, y) (sx, sy) = translate x y
                                         $ scale sx sy
                                         $ color col
                                         $ Text text

-- | render wall
renderWall :: Color       -- ^ Wall's color
       -> Width           -- ^ Wall's width
       -> Height          -- ^ Wall's height
       -> Position        -- ^ Wall's center
       -> Picture         -- ^ Wall's picture
renderWall col width height (x , y) = translate x y
                                    $ color col
                                    $ rectangleSolid width height

-- | render ball
renderBall :: Color       -- ^ Ball's color
       -> Radius          -- ^ Ball's radius
       -> (Float, Float)  -- ^ Ball's center
       -> Picture         -- ^ Picture of this ball
renderBall col radius (x, y) = translate x y
                             $ color col
                             $ circleSolid radius

-- | render brick
renderBrick :: Picture  -- ^ brick image
            -> Brick    -- ^ the brick to render
            -> Picture      -- ^ brick picture
renderBrick p b = translate x y p
      where
        (x, y) = brickLoc b



-- render paddle
renderPaddle :: Color -- ^ the paddle color
        -> Width      -- ^ paddle width
        -> Height     -- ^ paddle height
        -> Position   -- ^ paddle position
        -> Picture    -- ^ paddle picture
renderPaddle c w h (x, y) = translate x y
                          $ color c
                          $ rectangleSolid w h


-- | render a IO game
renderGameIO :: Game        -- ^ game to render
             -> IO Picture  -- ^ rendered game in IO
renderGameIO game = return $ renderGame game

  --loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/blue-rectangle-hi.bmp"


-- | render the game
renderGame :: Game      -- ^ The game state to render
           -> Picture   -- ^ A picture of this game state

-- MainMenu state
renderGame game @ Game { gameState = MainMenu } = pictures
      [ renderStateText orange "Haskell" (-120, 100) (0.5, 0.5)
      , renderStateText orange "Breakout" (-150, 0) (0.5, 0.5)
      , renderStateText orange "Press ENTER to continue" (-200, -100) (0.25, 0.25)
      ]

-- GameOver state
renderGame game @ Game { gameState = GameOver } = pictures
      [ renderStateText orange "Game Over" (-170, 0) (0.5, 0.5)
      , renderStateText orange "Score : " (-60 , -80) (0.25, 0.25)
      , renderStateText orange (show $ gameScore game)  (70, -80) (0.25, 0.25)
      , renderStateText orange "Press ANY key" (-100, -150) (0.25, 0.25)
      ]

-- Paused state
renderGame game @ Game { gameState = Paused } =
      renderStateText orange "PAUSED" (-120, 0) (0.5, 0.5)

-- Playing state
renderGame game @ Game { gameState = Playing } = pictures
      [ renderBall (dark red) 10 (ballLoc game)
      , renderWall wallColor gameWidth wallWidth wallUpPos
      , renderWall wallColor wallWidth gameHeight wallLeftPos
      , renderWall wallColor wallWidth gameHeight wallRightPos
      , renderPaddle paddleColor paddleWidth paddleHeight (paddleLoc $ paddle game)
      , pictures . fmap (renderBrick $ brickPicture game) $ bricks game
      , pictures . fmap (renderDot white 2) $ ballDots game
      , renderScore (gameScore game)
      ]
      where
        wallColor = blue
        brickColor = yellow
        paddleColor = cyan
