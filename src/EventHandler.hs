{-# LANGUAGE NamedFieldPuns #-}

module EventHandler where

  import GameBoard
  import Physics

  import Graphics.Gloss.Interface.Pure.Game
  import System.Exit

  -- -- | Respond to key events in IO.
  -- handleKeysIO :: (Event -> PongGame -> IO PongGame) -- ^ handleKeys function in IO
  --
  -- -- For an 'q' keypress, exit the game
  -- handleKeysIO (EventKey (Char 'q') Up _ _) game = exitSuccess
  --
  -- -- | Just return the same function as handleKeys but in IO
  -- handleKeysIO event game = return $ handleKeys event game
  --
  --
  -- -- | Pure responding to key events.
  -- handleKeys :: Event     -- ^ keyEvent
  --            -> PongGame  -- ^ Initial game state
  --            -> PongGame  -- ^ Game updated
  --
  -- -- For an 'r' keypress, reset the ball to the center.
  -- handleKeys (EventKey (Char 'r') _ _ _) game =
  --   game { ballLoc = (0, 0) }
  --
  -- -- For an 'p' keypress, pause the game.
  -- handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Playing } =
  --   game { gameState = Paused }
  -- handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Paused } =
  --   game { gameState = Playing }
  --
  -- -- For an 'Up' or 'Down' keypress, move verticaly player1 paddle
  -- handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
  --   game { player1v = 1 }
  -- handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game =
  --   game { player1v = 0 }
  -- handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
  --   game { player1v = -1 }
  -- handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game =
  --   game { player1v = 0 }
  --
  -- -- For an 'z' or 's' keypress, move verticaly player2 paddle
  -- handleKeys (EventKey (Char 'z') Down _ _) game =
  --   game { player2v = 1 }
  -- handleKeys (EventKey (Char 'z') Up _ _) game =
  --   game { player2v = 0 }
  -- handleKeys (EventKey (Char 's') Down _ _) game =
  --   game { player2v = -1}
  -- handleKeys (EventKey (Char 's') Up _ _) game =
  --   game { player2v = 0 }

  -- Do nothing for all other events.
  handleKeys _ game = game
