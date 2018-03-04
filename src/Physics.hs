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
      c = wallsCollision (ballLoc game) ballRadius gameWidth gameHeight

  -- | Detect a collision with one a the bricks still present. Upon collisions,
  --   update the bricks list
  bricksBounce :: Game  -- ^ Initial game state
               -> Game  -- ^ A new game state with an updated bricks
  bricksBounce game =
      case fst bc of
        Nothing         -> game
        Just TopSide    -> game {bricks = bricksUpdated, ballVel = (vx, -vy)}
        Just BottomSide -> game {bricks = bricksUpdated, ballVel = (vx, -vy)}
        Just LeftSide   -> game {bricks = bricksUpdated, ballVel = (-vx, vy)}
        Just RightSide  -> game {bricks = bricksUpdated, ballVel = (-vx, vy)}
      where
      (vx, vy) = ballVel game
      bc = bricksCollision (ballLoc game) ballRadius (bricks game)
      bricksUpdated = snd bc



--game {bricks = bricksUpdated, ballVel = (vx', vy')}
