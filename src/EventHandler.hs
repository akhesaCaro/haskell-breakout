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
              -> Game      -- ^ Initial game state
              -> Game      -- ^ Game updated
  -- For an 'Left' or 'Right' keypress, move verticaly player1 paddle
  handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
          game { paddlePos = (x - 10, y) }
          where
            (x, y) = paddlePos game
  handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
          game { paddlePos = (x + 10, y) }
          where
            (x, y) = paddlePos game

  -- Do nothing for all other events.
  handleKeys _ game = game
