module Levels
  ( isLevelOver
  ) where

import GameBoard
  -- | Checks if the bricks list is empty, next level

isLevelOver :: Game -> Game
isLevelOver game@Game {bricks = []} =
  if isWin
    then game {gameState = Win}
    else newLevelState (gameLevel + 1) score
  where
    gameLevel = level game
    score = gameScore game
    isWin = gameLevel > 9
isLevelOver game = game
