module Levels
  ( isLevelOver
  ) where

  import GameBoard


  -- | Checks if the bricks list is empty, next level
  isLevelOver :: Game -> Game
  isLevelOver game@ Game { bricks = [] } =  if isWin
                                            then game { gameState = Win }
                                            else initialState { level = gameLevel + 1
                                                              , gameState = Playing
                                                              }
        where gameLevel = level game
              isWin = gameLevel > 10
  isLevelOver game = game
