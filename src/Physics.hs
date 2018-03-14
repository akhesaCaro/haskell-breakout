{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , speedUp
  , movePaddle
  , computeDot
  , collisionBounce
  , bricksBounce
  , updatePaddleVel
  ) where

import GameBoard
import CollisionDetection

import Data.Maybe

import Debug.Trace


-- aliases
type Seconds = Float


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
moveBall :: Seconds   -- ^ The number of seconds since last Update
         -> Game      -- ^ The initial game state
         -> Game      -- ^ A new game state with an updated ball position
moveBall seconds game =
    game { ballLoc = (x' , y') }
    where
      -- Old locations and velocities
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      --New locations
      x' = x + vx * seconds
      y' = y + vy * seconds


-- | Detect collision (on walls or paddle) and change ball velocity
collisionBounce :: Seconds   -- ^ seconds since last update
                 -> Game     -- ^ initial state of the game
                 -> Game     -- ^ game updated
collisionBounce s game = game { ballVel = (nsX / s , nsY / s) }
      where
        ballD = ballDot game
        (ballVX, ballVY) = ballVel game
        (nsX, nsY)  = rectanglesDotCollision ballD (ballVX * s, ballVY * s) rectangles
        rectangles = [ (paddleLoc $ paddle game, paddleWidth, paddleHeight)
                     , (wallUpPos, gameWidth, wallWidth)
                     , (wallDownPos, gameWidth, wallWidth)
                     , (wallLeftPos, wallWidth, gameHeight)
                     , (wallRightPos, wallWidth, gameHeight)
                     ]

-- | Transform a brick to a rectangle
brickToRectangle :: Brick       -- ^ brick to transform
                 -> Rectangle   -- ^ brick rentangle
brickToRectangle b = (brickCenter, brickWidth, brickHeight)
      where
      brickCenter = brickLoc b


-- | Update the ball velocity and bricks if the ball hits a brick
bricksBounce :: Seconds   -- ^ Number of seconds since last update
              -> Game     -- ^ Initial state pf the game
              -> Game     -- ^ Game updated
bricksBounce s game = case fst bc of
      Nothing -> game
      Just (vx, vy) -> game {bricks = bricksUpdated, ballVel = speedUp (vx / s, vy / s)}
      where
        bc = bricksCollision (ballLoc game) (vx * s, vy * s) (bricks game)
        (vx, vy) = ballVel game
        bricksUpdated = snd bc


-- | Update the paddle position
movePaddle :: Game  -- ^ Initial game state
           -> Game  -- ^ Game paddle position updated
movePaddle game
      -- No step , no mouvement
      | vel == 0 = game
      -- Lefter than left wall, but trying to go right.
      | x - paddleWidth / 2  <= -(gameWidth / 2) + wallWidth / 2 && vel > 0 =
        let newLoc = (x + (paddleStep *  vel), y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
        -- You are going in the left wall
      | x - paddleWidth / 2  <= -(gameWidth / 2) + wallWidth / 2 && vel < 0 =
        let newLoc = (-(gameWidth / 2) + wallWidth / 2 + paddleWidth / 2, y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- Righter than right wall , but trying to go left.
      | x + paddleWidth / 2 >= gameWidth / 2 && vel < 0 =
        let newLoc = (x + (paddleStep *  vel), y) in
         game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- You are going in the right wall
      | x + paddleWidth / 2 >= gameWidth / 2 - wallWidth / 2 && vel > 0 =
         let newLoc = (gameWidth / 2 - wallWidth / 2 - paddleWidth / 2, y) in
         game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- Between the two walls
      | x - paddleWidth / 2 > -(gameWidth / 2) && x + paddleWidth / 2 < gameWidth / 2 =
        let newLoc = (x + (paddleStep *  vel), y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
      | otherwise = game
      where
        (x, y) = paddleLoc $ paddle game
        vel = fst $ paddleVel $ paddle game

-- | Update the paddle position
movePaddle' :: Game  -- ^ Initial game state
           -> Game  -- ^ Game paddle position updated
movePaddle' game
      -- | No step , no mouvement
      | vel == 0 = game
      -- |
      | otherwise =
              let newLoc = (x + (paddleStep *  vel), y) in
              game { paddle = (paddle game) { paddleLoc = newLoc }}
      where
        (x, y) = paddleLoc $ paddle game
        vel = fst $ paddleVel $ paddle game

updatePaddleVel :: Seconds
                -> Game
                -> Game
updatePaddleVel seconds game = case newSpeed of
            Nothing -> game
            Just (nsX, nsY) -> game { paddle = (paddle game) { paddleVel = (nsX / seconds , nsY / seconds) } }
        where
          (x, y) = paddleLoc $ paddle game
          (vx, vy) = paddleVel $ paddle game
          dotLoc :: Position
          dotLoc = (x - paddleWidth / 2, y)
          dotSpeed :: Speed
          dotSpeed = (vx * seconds, vy * seconds)
          rectangle :: Rectangle
          rectangle = ((x, y), paddleWidth, paddleHeight)
          newSpeed = traceShowId $ rectangleDotCollision dotLoc dotSpeed rectangle
