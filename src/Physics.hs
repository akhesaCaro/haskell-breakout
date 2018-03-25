{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , speedUp
  , movePaddle
  , computeDot
  , collisionBounce
  -- , bricksBounce
  , isGameOver
  -- , paddleBounce
  , resetPaddleVel
  ) where


import Data.List (sortOn)
import GameBoard
import CollisionDetection

import Data.Maybe

-- aliases
type Seconds = Float


-- Speed up the ball
speedUp :: Position -> Position
speedUp (x, y) = (speedRatio * x, speedRatio * y)


-- -- | Update the dot position with the velocity
-- computeDots :: Game  -- ^ current game state
--             -> Game  -- ^ game updated
-- computeDots game = game { ballDots = computeDot radius ballV <$> ballDots game }
--       where
--         ballL = ballLoc game
--         ballV = ballVel game
--         radius = ballRadius

-- computeDot :: Radius
--            -> Velocity
--            -> Position
--            -> Position
-- computeDot radius (vx, vy) (x, y) = (ax + x , ay + y)
--       where
--         ax = (vx * radius) / normV
--         ay = (vy * radius) / normV
--         normV = sqrt ( vx * vx + vy * vy )

-- | Update the dot position with the velocity
computeDot :: Game  -- ^ current game state
           -> Game  -- ^ game updated
computeDot game = game { ballDots = [(ax + x , ay + y)] }
      where
        (x, y)  = ballLoc game
        (vx, vy) = ballVel game
        radius = ballRadius
        ax = (vx * radius) / normV
        ay = (vy * radius) / normV
        normV = sqrt ( vx * vx + vy * vy )

-- | Update the ball position using its current velocity.
moveBall :: Seconds   -- ^ The number of seconds since last Update
         -> Game      -- ^ current game state
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


-- | Detect collision on the walls and change ball velocity
collisionBounce :: Seconds   -- ^ seconds since last update
                 -> Game     -- ^ current game state
                 -> Game     -- ^ game updated
collisionBounce s game =
      case collisions of
          [] -> game
          ((t, TopSide):xs) -> game { ballVel = (ballVX, -ballVY) }
          ((t, BottomSide):xs) -> game { ballVel = (ballVX, -ballVY) }
          ((t, RightSide):xs) -> game { ballVel = (-ballVX, ballVY) }
          ((t, LeftSide):xs) -> game { ballVel = (-ballVX, ballVY) }
      where
        dots = ballDots game
        (ballVX, ballVY) = ballVel game
        speed = (ballVX * s, ballVY * s)
        gameWalls = [ (wallUpPos, gameWidth, wallWidth)
                , (wallLeftPos, wallWidth, gameHeight)
                , (wallRightPos, wallWidth, gameHeight)
                ]
        gameBricks = brickToRectangle <$> bricks game
        gamePaddle = paddleToRectangle $ paddle game
        rectangles = [gamePaddle] ++ gameWalls ++ gameBricks
        collisions = sortOn fst . catMaybes $ (detectDotsCollision speed dots <$> rectangles)



-- | reset paddle velocity when the mouse stops
resetPaddleVel :: Game   -- ^ current game state
               -> Game   -- ^ game updated
resetPaddleVel game = if mouseEvent game
                        then game { mouseEvent = False }
                        else game { paddle = (paddle game) { paddleVel = (0,0)} }



-- | Update the paddle position aand stop it if it goes throught the wall
movePaddle :: Game  -- ^ current game state
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
        paddleStep = 1


-- | Verify if the game is over (ball outside the game)
isGameOver :: Game  -- ^ current game state
           -> Game  -- ^ updated game
isGameOver game
      | y < -(gameHeight / 2) = game {gameState = GameOver}
      | otherwise = game
      where
        (_, y) = ballLoc game
