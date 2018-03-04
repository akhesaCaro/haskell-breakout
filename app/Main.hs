module Main where

import           EventHandler
import           GameBoard
import           Physics
import           Rendering

import  Graphics.Gloss
import  Graphics.Gloss.Data.ViewPort

-- ** Background Color
background :: Color
background = black



-- | Update the game by moving the ball and bouncing off walls.
update :: Float     -- ^ The number of seconds since last update
       -> Game  -- ^ The intial game state
       -> Game  -- ^ A new game state with an updated ball and paddles positions.
update seconds = wallBounce . moveBall seconds


-- drawing :: Picture
-- drawing = pictures
--   [ mkWall wallColor (fromIntegral winWith) wallWidth wallUpPos
--   , mkWall wallColor (fromIntegral winWith) wallWidth wallDownPos
--   , mkWall wallColor wallWidth (fromIntegral winHeight) wallLeftPos
--   , mkWall wallColor wallWidth (fromIntegral winHeight) wallRightPos
--   ]
--   where
--     wallColor = blue
--     ballColor = dark red
--     paddleColor = light (light blue)

-- * Window
window :: Display
window = InWindow "Haskell Breakout" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState renderGame handleKeys update
