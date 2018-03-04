{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , wallBounce
  , bricksBounce
  ) where

  import GameBoard
  import CollisionDetection

  import Data.Maybe

  -- | Update the ball position using its current velocity.
  moveBall :: Float -- ^ The number of seconds since last Update
           -> Game  -- ^ The initial game state
           -> Game  -- ^ A new game state with an updated ball position
  moveBall seconds game =
      game { ballLoc = (x' , y') }
      where
        -- Old locations and velocities
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        --New locations
        x' = x + vx * seconds
        y' = y + vy * seconds


  -- * Wall Universe

  -- | Detect a collision with one of the side walls. Upon collisions,
  --   update the velocity of the ball to bounce it off the wall.
  wallBounce :: Game  -- ^ The initial game state
             -> Game  -- ^ A new game state with an updated ball velocity
  wallBounce game = case c of
      Just BottomSide    -> game { ballVel = (vx, -vy) }
      Just TopSide -> game { ballVel = (vx, -vy) }
      Just RightSide   -> game { ballVel = (-vx, vy) }
      Just LeftSide  -> game { ballVel = (-vx, vy) }
      Nothing         -> game
      where
      (vx, vy) = ballVel game
      c = wallsCollision (ballLoc game) ballRadius gameWidth gameHeight

  -- * Brick Universe

  -- Speed up the ball velocity
  speedUp :: (Float, Float) -> (Float, Float)
  speedUp (x, y) = (speedRatio * x, speedRatio * y)

  -- | Detect a collision with one a the bricks still present. Upon collisions,
  --   update the bricks list
  bricksBounce :: Game  -- ^ Initial game state
               -> Game  -- ^ A new game state with an updated bricks
  bricksBounce game = case fst bc of
        Nothing         -> game
        Just TopSide    -> game {bricks = bricksUpdated, ballVel = speedUp (vx, -vy)}
        Just BottomSide -> game {bricks = bricksUpdated, ballVel = speedUp (vx, -vy)}
        Just LeftSide   -> game {bricks = bricksUpdated, ballVel = speedUp (-vx, vy)}
        Just RightSide  -> game {bricks = bricksUpdated, ballVel = speedUp (-vx, vy)}
        where
        -- ball position
        (vx, vy) = ballVel game
        -- a collision? where?
        bc = bricksCollision (ballLoc game) ballRadius (bricks game)
        -- bricks list
        bricksUpdated = snd bc
