module Rendering
  (renderGame
  , Library (..)
  ) where

import GameBoard

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)

-- | colors
scoreColor      = orange
levelColor      = orange
paddleColor     = dark $ dark violet
stateText       = orange
wallColor       = violet
ballColor       = orange
stateBGColor    = black
itemColor       = yellow

-- | Images library
data Library = Library
      { brickImg        :: Picture
      , mainMenuImg     :: Picture
      , winImg          :: Picture
      , gameOverImg     :: Picture
      , nextLevelImg    :: Picture
      , haskellLogoImg  :: Picture
      , pausedImg       :: Picture
      }

-- | Render score
renderScore :: Color    -- ^ color
            -> Score    -- ^ score to render
            -> Picture  -- ^ picture of the score
renderScore c s = translate (- (gameWidth / 2) + 15) ((gameHeight / 2) - 50)
              $ scale 0.15 0.15
              $ color c
              $ Text ("Score : " ++ show s)

-- | Render level
renderGameLevel :: Color      -- ^ color
                -> GameLevel  -- ^ level to render
                -> Picture    -- ^ picture of the level
renderGameLevel c l = translate ((gameWidth / 2) - 110) ((gameHeight / 2) - 50)
              $ scale 0.15 0.15
              $ color c
              $ Text ("Level : " ++ show l)

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

-- | render item
renderItem :: Color     -- ^ item color
           -> Item      -- ^ item to render
           -> Picture   -- ^ item rendered
renderItem col item = translate x y
                        $ color col
                        $ rectangleSolid itemWidth itemHeight
                        where
                          (x, y) = itemPos item

-- | render ball
renderBall :: Color       -- ^ Ball's color
       -> Radius          -- ^ Ball's radius
       -> Position  -- ^ Ball's center
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

-- | render background
renderBackground :: Color     -- ^ background color
                 -> Picture   -- ^ picture to render
renderBackground c = color c
                    $ rectangleSolid gameWidth gameHeight

-- | render the game
renderGame :: Game      -- ^ The game state to render
           -> Library   -- ^ image library
           -> Picture   -- ^ A picture of this game state

-- MainMenu state
renderGame game @ Game { gameState = MainMenu } library = mainMenuImg library

-- GameOver state
renderGame game @ Game { gameState = GameOver } library = pictures
      [ gameOverImg library
      , renderStateText stateText (show $ level game) (-0,-175) (0.25, 0.25)
      , renderStateText stateText (show $ gameScore game) (-20,-300) (0.25, 0.25)
      ]

-- Win state
renderGame game @ Game { gameState = Win } library = pictures
        [ winImg library
        , renderStateText stateText (show $ level game) (-0,-175) (0.25, 0.25)
        , renderStateText stateText (show $ gameScore game) (-20,-300) (0.25, 0.25)
        ]

-- Paused state
renderGame game @ Game { gameState = Paused } library = pausedImg library


-- Between two levels state
renderGame game @ Game { gameState = NextLevel } library = pictures
        [ nextLevelImg library
        , renderStateText stateText (show $ level game) (0,-125) (0.25, 0.25)
        , renderStateText stateText (show $ gameScore game) (0,-275) (0.25, 0.25)
        ]

-- Playing state
renderGame game @ Game { gameState = Playing } library = pictures
      [ renderBall ballColor 10 (ballLoc game)
      , renderWall wallColor gameWidth wallWidth wallUpPos
      , renderWall wallColor wallWidth (gameHeight + wallWidth * 2) wallLeftPos
      , renderWall wallColor wallWidth (gameHeight + wallWidth * 2) wallRightPos
      , renderPaddle paddleColor paddleW paddleHeight (paddleLoc $ paddle game)
      , pictures . fmap (renderBrick $ brickImg library) $ bricks game
      -- , pictures . fmap (renderDot white 2) $ ballDots game
      , pictures . fmap (renderItem itemColor) $ items game
      , renderScore scoreColor (gameScore game)
      , renderGameLevel levelColor (level game)
      ]
      where
        paddleW = paddleWidth $ paddle game
