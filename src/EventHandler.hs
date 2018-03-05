{-# LANGUAGE NamedFieldPuns #-}

module EventHandler
  ( handleKeys
  ) where

  import GameBoard
  import Physics

  import Graphics.Gloss.Interface.Pure.Game
  import System.Exit

  -- Do nothing for all other events.
  handleKeys _ game = game
