module Lib
  ( World(..)
  , renderWorldIO
  , handleKeysWorldIO
  ) where

import EventHandler
import GameBoard
import Rendering

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)

-- | World (with the game and the image library)
type World = (Game, Library)

-- | render IO World
renderWorldIO :: World -> IO Picture
renderWorldIO (game, lib) = return $ renderGame game lib

-- | IO responding to key events for the world
handleKeysWorldIO :: Event -> World -> IO World
handleKeysWorldIO e w@(g, l) = do
  game <- handleKeysIO e (fst w)
  return (game, l)
