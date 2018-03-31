module Main where

import  EventHandler
import  GameBoard
import  Physics
import  Rendering

-- Conflict with the type Vector in module Maths
import  Graphics.Gloss hiding (Vector)

import  Graphics.Gloss.Data.ViewPort
import  Graphics.Gloss.Interface.IO.Game

-- | Window background Color
background :: Color
background = black

-- | update de the game in IO
updateIO :: Float
         -> Game
         -> IO Game
updateIO seconds game = return $ update seconds game

-- | Update the game by moving the ball and bouncing off walls.
update :: Float   -- ^ The number of seconds since last update
       -> Game    -- ^ current game state
       -> Game    -- ^ A new game state with an updated ball and paddles positions.
-- Game playing
update seconds game @ Game { gameState = Playing }
      = isGameOver
      . resetPaddleVel
      . movePaddle
      . paddleBounce seconds
      . bricksBounce seconds
      . moveBall seconds
      . wallsBounce seconds
      . bricksBounce seconds
      . computeDots $ game

-- Game in a main menu state / Game paused
update _ game  = game

-- | Window
window :: Display
window = InWindow "Haskell Breakout" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60

-- | Main
main :: IO ()
main = playIO window background fps initialState renderGameIO handleKeysIO updateIO
