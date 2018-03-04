module Rendering where

  import GameBoard

  import Graphics.Gloss


  -- * make state text
  mkStateText :: Color -> String -> Float -> Float -> Picture
  mkStateText col text x y = translate (-120) 0 $ scale x y $ color col $ Text text

  -- * make a wall
  mkWall :: Color           -- ^ Wall's color
         -> Float           -- ^ Wall's width
         -> Float           -- ^ Wall's height
         -> (Float, Float)  -- ^ Wall's center
         -> Picture         -- ^ Wall's picture
  mkWall col width height (x , y) = translate x y $ color col $ rectangleSolid width height

  -- * make a ball
  mkBall :: Color   -- ^ Ball's color
         -> Float   -- ^ Ball's radius
         -> (Float, Float)  -- ^ Ball's center
         -> Picture         -- ^ Picture of this ball
  mkBall col radius (x, y) = translate x y  $ color col $ circleSolid radius

  -- * make a brick
  mkBrick :: Brick    -- ^ the brick to render
          -> Picture  -- ^ brick picture
  mkBrick b = translate x y $ color (brickCol b) $ rectangleSolid brickWidth brickHeight
      where
        (x, y) = brickLoc b

  -- * render the game
  renderGame :: Game    -- ^ The game state to render
             -> Picture -- ^ A picture of this game state
  renderGame game = pictures
      [ mkBall (dark red) 10 (ballLoc game)
      , mkWall wallColor (fromIntegral winWidth - wallWidth / 2) wallWidth wallUpPos
      , mkWall wallColor (fromIntegral winWidth + wallWidth / 2) wallWidth wallDownPos
      , mkWall wallColor wallWidth (fromIntegral winHeight + wallWidth / 2) wallLeftPos
      , mkWall wallColor wallWidth (fromIntegral winHeight - wallWidth / 2) wallRightPos
      , pictures . fmap mkBrick $ bricks game
      ]
      where
        wallColor = blue
        brickColor = yellow
