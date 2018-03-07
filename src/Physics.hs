{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , wallBounce
  , bricksBounce
  , speedUp
  , paddleBounce
  , movePaddle
  , computeDot
  ) where

  import GameBoard
  import CollisionDetection

  import Data.Maybe

  -- * Ball Universe

  -- Speed up the ball
  speedUp :: Position -> Position
  speedUp (x, y) = (speedRatio * x, speedRatio * y)


  -- | Update the dot position with the velocity
  computeDot :: Game  -- ^ initial game state
             -> Game  -- ^ game updated
  computeDot game = game { ballDot = (ax + x , ay + y) }
        where
          (x, y)  = ballLoc game
          (vx, vy) = ballVel game
          radius = ballRadius
          ax = (vx * radius) / normV
          ay = (vy * radius) / normV
          normV = sqrt ( vx * vx + vy * vy )



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


  -- * Wall Universe

  -- | Detect a collision with one of the side walls. Upon collisions,
  --   update the velocity of the ball to bounce it off the wall.
  wallBounce :: Game  -- ^ The initial game state
             -> Game  -- ^ A new game state with an updated ball velocity
  wallBounce game = case c of
      Just BottomSide   -> game { ballVel = (vx, -vy) }
      Just TopSide      -> game { ballVel = (vx, -vy) }
      Just RightSide    -> game { ballVel = (-vx, vy) }
      Just LeftSide     -> game { ballVel = (-vx, vy) }
      Nothing           -> game
      where
      (vx, vy) = ballVel game
      c = wallsCollision (ballLoc game) ballRadius gameWidth gameHeight

  -- * Brick Universe

  -- | Detect a collision with one of the bricks list. Upon collisions,
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

  -- * Paddle Universe

  -- | Detec a collision with the paddle. Upon collision, update ball velocity
  paddleBounce :: Game  -- ^ Intial game state
               -> Game  -- ^ Game with ball velocity updated
  paddleBounce game =
    case pc of
      Nothing   -> game
      Just TopSide    -> game { ballVel = (vx, -vy)}
      Just BottomSide -> game { ballVel = (vx, -vy)}
      Just LeftSide   -> game { ballVel = (-vx, vy)}
      Just RightSide  -> game { ballVel = (-vx, vy)}
    where
        (vx, vy) = ballVel game
        pc = paddleCollision (ballLoc game) ballRadius (paddleLoc $ paddle game) paddleWidth paddleHeight


  -- | Update the paddle position
  movePaddle :: Game  -- ^ Initial game state
             -> Game  -- ^ Game paddle position updated
  movePaddle game
        -- | No step , no mouvement
        | vel == 0 = game
        -- | Lefter than left wall, but trying to go right.
        | x - paddleWidth / 2  <= -(gameWidth / 2) && vel > 0 =
          let newLoc = (x + (paddleStep *  vel), y) in
          game { paddle = (paddle game) { paddleLoc = newLoc }}
        -- | Righter than right wall , but trying to go left.
        | x + paddleWidth / 2 >= gameWidth / 2 && vel < 0 =
          let newLoc = (x + (paddleStep *  vel), y) in
          game { paddle = (paddle game) { paddleLoc = newLoc }}
        -- | Between the two walls
        | x - paddleWidth / 2 > -(gameWidth / 2) && x + paddleWidth / 2 < gameWidth / 2 =
          let newLoc = (x + (paddleStep *  vel), y) in
          game { paddle = (paddle game) { paddleLoc = newLoc }}
        | otherwise = game
        where
          (x, y) = paddleLoc $ paddle game
          vel = fst $ paddleVel $ paddle game
