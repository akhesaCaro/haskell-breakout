{-# LANGUAGE NamedFieldPuns #-}

module EventHandler
  ( handleKeys
  ) where

import GameBoard
import Physics

import Graphics.Gloss.Interface.Pure.Game
import System.Exit

-- | Pure responding to key events.
handleKeys :: Event     -- ^ keyEvent
           -> Game      -- ^ current game state
           -> Game      -- ^ Game updated
-- For an 'Left' or 'Right' keypress, move verticaly player1 paddle
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
      game { paddle = (paddle game) { paddleVel = (-1 , 0) }}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =
      game { paddle = (paddle game) { paddleVel = (0 , 0) }}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
      game { paddle = (paddle game) { paddleVel = (1 , 0) }}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =
      game { paddle = (paddle game) { paddleVel = (0 , 0) }}

-- For an 'p' keypress, pause the game.
handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Playing } =
      game { gameState = Paused }
handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Paused } =
      game { gameState = Playing }

-- For an 'enter' keypress, start the game.
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@ Game { gameState = MainMenu } =
      game { gameState = Playing }

-- Do nothing for all other events.
handleKeys _ game = game
