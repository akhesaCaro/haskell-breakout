{-# LANGUAGE ScopedTypeVariables #-}

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
background = greyN 0.5

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
main = do
  (pictureImage :: Picture) <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/blue-rectangle-hi.bmp"
  playIO window background fps (initialState pictureImage) renderGameIO handleKeysIO updateIO
