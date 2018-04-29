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
background = greyN 0.1


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

-- | load library
loadLibrary :: IO Library
loadLibrary = do
      (brickImg :: Picture)       <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/purpleBrick.bmp"
      (mainMenuImg :: Picture)    <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/mainMenu.bmp"
      (winImg :: Picture)         <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/win.bmp"
      (gameOverImg :: Picture)    <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/gameOver.bmp"
      (nextLevelImg :: Picture)   <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/nextLevel.bmp"
      (haskellLogoImg :: Picture) <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/haskellLogo.bmp"
      (pausedImg :: Picture)      <- loadBMP "/media/akhesa/16e1988c-fe98-4a2e-b528-320abdc3d132/akhesa/projects/haskell-breakout/library/paused.bmp"
      return Library { brickImg = brickImg
                      , mainMenuImg = mainMenuImg
                      , winImg = winImg
                      , gameOverImg = gameOverImg
                      , nextLevelImg = nextLevelImg
                      , haskellLogoImg = haskellLogoImg
                      , pausedImg = pausedImg
                      }

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
  library <- loadLibrary
  playIO' window background fps (initialState, library) renderWorldIO handleKeysWorldIO updateWorldIO
