module Main where

import  EventHandler
import  GameBoard
import  Physics
import  Rendering

import  Graphics.Gloss
import  Graphics.Gloss.Data.ViewPort

-- | Window Background Color
background :: Color
background = black

-- | Update the game by moving the ball and bouncing off walls.
update :: Float   -- ^ The number of seconds since last update
       -> Game    -- ^ The intial game state
       -> Game    -- ^ A new game state with an updated ball and paddles positions.
update seconds =
      computeDot . paddleBounce . movePaddle . bricksBounce . wallBounce . moveBall seconds

-- | Window
window :: Display
window = InWindow "Haskell Breakout" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60

-- | Main
main :: IO ()
main = play window background fps initialState renderGame handleKeys update
