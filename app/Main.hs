{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import  Lib
import  EventHandler
import  GameBoard
import  Physics
import  Rendering
import  Levels

-- Conflict with the type Vector in module Maths
import  Graphics.Gloss hiding (Vector)

import  Graphics.Gloss.Data.ViewPort
import  Graphics.Gloss.Interface.IO.Game


-- | Window background Color
background :: Color
background = greyN 0.5


-- | update world
updateWorldIO :: Float
              -> World
              -> IO World
updateWorldIO s w@(g, l) = do
      game <- updateIO s (fst w)
      return (game, l)

-- | update de the game in IO
updateIO :: Float
         -> Game
         -> IO Game
updateIO seconds game = return $ update seconds game


-- | Update the game state
update :: Float   -- ^ The number of seconds since last update
       -> Game    -- ^ current game state
       -> Game    -- ^ A new game state with an updated ball and paddles positions.
-- Game playing
update seconds game @ Game { gameState = Playing }
      = isGameOver
      . isLevelOver
      . itemsBounce
      . moveItems
      . resetPaddleVel
      . movePaddle
      . paddleBounce seconds
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
playIO' :: Display
       -> Color
       -> Int
       -> World
       -> (World -> IO Picture)
       -> (Event -> World -> IO World)
       -> (Float -> World -> IO World)
       -> IO ()
playIO' = playIO

main :: IO ()
main = do
  (pictureImage :: Picture) <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/purpleBrick.bmp"
  playIO' window background fps (initialState, Library {brickImg = pictureImage}) renderWorldIO handleKeysWorldIO updateWorldIO
