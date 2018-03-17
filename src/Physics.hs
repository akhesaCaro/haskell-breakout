{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , speedUp
  , movePaddle
  , computeDot
  , collisionBounce
  , bricksBounce
  , isGameOver
  ) where

import GameBoard
import CollisionDetection

import Data.Maybe



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
                  --   , (wallDownPos, gameWidth, wallWidth)
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
      Just (vx, vy) -> game { bricks    = bricksUpdated
                            , ballVel   = speedUp (vx / s, vy / s)
                            , gameScore = addScore score }
      where
        bc = bricksCollision (ballLoc game) (vx * s, vy * s) (bricks game)
        (vx, vy) = ballVel game
        bricksUpdated = snd bc
        score = gameScore game


-- | Update the paddle position aand stop it if it goes throught the wall
movePaddle :: Game  -- ^ Initial game state
           -> Game  -- ^ Game paddle position updated
movePaddle game
      -- No step , no mouvement
      | vel == 0 = game
      -- Lefter than left wall, but trying to go right.
      | x - halfPaddle  <= leftGameBorder && vel > 0 =
        let newLoc = (x + (paddleStep *  vel), y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- You are going into the left wall
      | x - halfPaddle  <= leftGameBorder && vel < 0 =
        let newLoc = (leftGameBorder + halfPaddle, y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- Righter than right wall , but trying to go left.
      | x + halfPaddle >= rightGameBorder && vel < 0 =
        let newLoc = (x + (paddleStep *  vel), y) in
         game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- You are going into the right wall
      | x + halfPaddle >= rightGameBorder && vel > 0 =
         let newLoc = (rightGameBorder - halfPaddle, y) in
         game { paddle = (paddle game) { paddleLoc = newLoc }}
      -- Between the two walls
      | x - halfPaddle > leftGameBorder && x + halfPaddle < rightGameBorder =
        let newLoc = (x + (paddleStep *  vel), y) in
        game { paddle = (paddle game) { paddleLoc = newLoc }}
      | otherwise = game
      where
        (x, y) = paddleLoc $ paddle game
        vel = fst $ paddleVel $ paddle game
        halfPaddle = paddleWidth / 2
        leftGameBorder = -(gameWidth / 2) + wallWidth / 2
        rightGameBorder = gameWidth / 2 - wallWidth / 2


-- | Verify if the game is over (ball outside the game)
isGameOver :: Game  -- ^ Intial game state
           -> Game  -- ^ Game updated
isGameOver game
      | y < -(gameHeight / 2) = game {gameState = GameOver}
      | otherwise = game
      where
        (_, y) = ballLoc game
