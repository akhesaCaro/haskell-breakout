module Levels
  ( isLevelOver
  ) where

  import GameBoard

  -- | Checks if the bricks list is empty, if so win state
  isLevelOver :: Game -> Game
  isLevelOver game@ Game { bricks = [] } = game { gameState = Win }
  isLevelOver g = g
