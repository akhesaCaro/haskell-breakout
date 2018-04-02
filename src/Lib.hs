module Lib
    ( World (..)
    , renderWorldIO
    , handleKeysWorldIO
    ) where

import GameBoard
import Rendering
import EventHandler

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)

-- | World (with the game and the image library)
type World = (Game, Library)


-- | render IO World
renderWorldIO :: World
              -> IO Picture
renderWorldIO w = renderGameIO (fst w)


-- | IO responding to key events for the world
handleKeysWorldIO :: Event
                  -> World
                  -> IO World
handleKeysWorldIO e w@(g, l) = do
      game <- handleKeysIO e (fst w)
      return (game, l)
