{-# LANGUAGE NamedFieldPuns #-}

module Physics where

  import GameBoard
  import CollisionDetection

  import Data.Maybe


  -- * Ball Universe

  -- | Update the ball position using its current velocity.
  moveBall :: Float     -- ^ The number of seconds since last Update
           -> Game  -- ^ The initial game state
           -> Game  -- ^ A new game state with an updated ball position


  -- -- When paused, don't move.
  -- moveBall _ game@ Game { paused } | paused = game

  -- Moving the ball.
  moveBall seconds game =
    game { ballLoc = (x' , y') }
    where
      -- Old locations and velocities
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      --New locations
      x' = x + vx * seconds
      y' = y + vy * seconds


  -- -- | Detect a collision with a paddle. Upon collisions,
  -- -- change the velocity of the ball to bounce it off the paddle.
  -- paddleBounce :: Game  -- ^ The initial game state
  --              -> Game  -- ^ A new game state with an updated ball velocity
  --
  -- paddleBounce game = game { ballVel = (vx', vy) }
  --   where
  --       (vx, vy) = ballVel game
  --
  --       vx' = if paddleCollision game
  --             then
  --                 -- Update the velocity
  --                 (-vx)
  --
  --                 else
  --                 -- Do nothing.Return te old velocity
  --                 vx

  -- | Detect a collision with one of the side walls. Upon collisions,
  -- update the velocity of the ball to bounce it off the wall.
  wallBounce :: Game  -- ^ The initial game state
             -> Game  -- ^ A new game state with an updated ball velocity

  wallBounce game =
      case c of
        Just TopWall -> game { ballVel = (vx, -vy) }
        Just BottomWall -> game { ballVel = (vx, -vy) }
        Just LeftWall -> game { ballVel = (-vx, vy) }
        Just RightWall -> game { ballVel = (-vx, vy) }
        Nothing -> game
    where
      (vx, vy) = ballVel game
      c = wallsCollision (ballLoc game) ballRadius
            (fromIntegral winWidth - wallWidth / 2) (fromIntegral winHeight - wallWidth / 2)
