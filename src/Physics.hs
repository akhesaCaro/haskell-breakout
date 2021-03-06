{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , speedUp
  , movePaddle
  , computeBallDots
  , wallsBounce
  , bricksBounce
  , isGameOver
  , paddleBounce
  , resetPaddleVel
  , moveItems
  , itemsBounce
  ) where

import CollisionDetection
import Data.List (sortOn)
import Data.Maybe
import GameBoard

import Maths

-- aliases
type Seconds = Float

-- Speed up the ball
speedUp :: Position -> Position
speedUp (x, y) = (speedRatio * x, speedRatio * y)

-- | update the 3 dots position used to calculate the collision vectors
computeBallDots ::
     Game -- ^ current state game
  -> Game -- ^ game updated
computeBallDots game = game {ballDots = [dot1, dot2, dot3]}
  where
    dot1 = computeBallDot (ballLoc game) (ballVel game) ballRadius
    dot2 = computeBallDot (ballLoc game) nv ballRadius
      where
        nv = matrixMultiplication ((0, 1), (-1, 0)) (ballVel game)
    dot3 = computeBallDot (ballLoc game) nv ballRadius
      where
        nv = matrixMultiplication ((0, -1), (1, 0)) (ballVel game)

-- | Update the dot position with the velocity
computeBallDot ::
     Position -- ^ ball center
  -> Velocity -- ^ vector
  -> Radius -- ^ ball radius
  -> Position -- ^ dot position
computeBallDot (x, y) (vx, vy) radius = (ax + x, ay + y)
  where
    ax = (vx * radius) / normV
    ay = (vy * radius) / normV
    normV = sqrt (vx * vx + vy * vy)

-- | Update the ball position using its current velocity.
moveBall ::
     Seconds -- ^ The number of seconds since last Update
  -> Game -- ^ current game state
  -> Game -- ^ A new game state with an updated ball position
moveBall seconds game = game {ballLoc = (x', y')}
        -- Old locations and velocities
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
        -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Update the paddle position aand stop it if it goes throught the wall
movePaddle ::
     Game -- ^ current game state
  -> Game -- ^ Game paddle position updated
movePaddle game
      -- No step , no mouvement
  | vel == 0 = game
      -- Lefter than left wall, but trying to go right.
  | x - halfPaddle <= leftGameBorder && vel > 0 =
    let newLoc = (x + (paddleStep * vel), y)
    in game {paddle = (paddle game) {paddleLoc = newLoc}}
      -- You are going into the left wall
  | x - halfPaddle <= leftGameBorder && vel < 0 =
    let newLoc = (leftGameBorder + halfPaddle, y)
    in game {paddle = (paddle game) {paddleLoc = newLoc}}
      -- Righter than right wall , but trying to go left.
  | x + halfPaddle >= rightGameBorder && vel < 0 =
    let newLoc = (x + (paddleStep * vel), y)
    in game {paddle = (paddle game) {paddleLoc = newLoc}}
      -- You are going into the right wall
  | x + halfPaddle >= rightGameBorder && vel > 0 =
    let newLoc = (rightGameBorder - halfPaddle, y)
    in game {paddle = (paddle game) {paddleLoc = newLoc}}
      -- Between the two walls
  | x - halfPaddle > leftGameBorder && x + halfPaddle < rightGameBorder =
    let newLoc = (x + (paddleStep * vel), y)
    in game {paddle = (paddle game) {paddleLoc = newLoc}}
  | otherwise = game
  where
    (x, y) = paddleLoc $ paddle game
    vel = fst $ paddleVel $ paddle game
    halfPaddle = paddleWidth (paddle game) / 2
    leftGameBorder = -(gameWidth / 2) + wallWidth / 2
    rightGameBorder = gameWidth / 2 - wallWidth / 2
    paddleStep = 1

itemsBounce :: Game -> Game
itemsBounce game = go itemTypeList game
  where
    (itemsUpdated, itemTypeList) = itemsCollision (paddle game) (items game)
    go :: [ItemType] -> Game -> Game
    go [] game = game {items = itemsUpdated}
    go (PaddleExpander:xs) game =
      go xs game {paddle = p {paddleWidth = paddleW + 10}}
      where
        p = paddle game
        paddleW = paddleWidth p
    go (PaddleMinifier:xs) game =
      go xs game {paddle = p {paddleWidth = paddleW - 10}}
      where
        p = paddle game
        paddleW = paddleWidth p

-- | Update the items positions
moveItems ::
     Game -- ^ game to update
  -> Game -- ^ game updated
moveItems game = game {items = itemsUpdated}
  where
    itemsUpdated = catMaybes . fmap (moveItem itemVel) $ items game

-- | update item position
moveItem ::
     Velocity -- ^ item velocity
  -> Item -- ^ item to move
  -> Maybe Item -- ^ item with a new position
moveItem (velx, vely) i
  | y < -(gameHeight / 2) = Nothing
  | otherwise = Just i {itemPos = (x + velx, y + vely)}
  where
    (x, y) = itemPos i

--  | Update the ball velocity and bricks if the ball hits a brick
bricksBounce ::
     Seconds -- ^ Number of seconds since last update
  -> Game -- ^ current state pf the game
  -> Game -- ^ Game updated
bricksBounce s game =
  case bc of
    Nothing -> game
    Just (vx, vy) ->
      game
      { bricks = bricksUpdated
      , ballVel = speedUp (vx / s, vy / s)
      , gameScore = addScore score
      , items = itemsUpdated ++ itemLts
      }
  where
    (bc, bricksUpdated, itemsUpdated) =
      bricksCollision (vx * s, vy * s) (ballDots game) (bricks game)
    (vx, vy) = ballVel game
    score = gameScore game
    itemLts = items game

-- | Detect collision on the paddle and change velocity and score
paddleBounce ::
     Seconds -- ^ second since last update
  -> Game -- ^ current game state
  -> Game -- ^ game updated
paddleBounce s game =
  case nws of
    Nothing -> game
    Just t -> game {ballVel = (nsX / s + (paddleV * 50), nsY / s)}
      where (nsX, nsY) = collisionToSpeed t (ballVX * s, ballVY * s)
  where
    paddleV = fst $ paddleVel $ paddle game
    dots = ballDots game
    (ballVX, ballVY) = ballVel game
    nws =
      detectDotsCollision (ballVX * s, ballVY * s) dots $
      paddleToRectangle $ paddle game

-- | Detect collision on the walls and change ball velocity
wallsBounce ::
     Seconds -- ^ seconds since last update
  -> Game -- ^ current game state
  -> Game -- ^ game updated
wallsBounce s game =
  case collisions of
    [] -> game
    (x:xs) -> game {ballVel = collisionToSpeed x (ballVX, ballVY)}
  where
    dots = ballDots game
    (ballVX, ballVY) = ballVel game
    speed = (ballVX * s, ballVY * s)
    gameWalls =
      [ (wallUpPos, gameWidth, wallWidth)
      , (wallLeftPos, wallWidth, gameHeight)
      , (wallRightPos, wallWidth, gameHeight)
      ]
    collisions =
      sortOn fst . catMaybes $ (detectDotsCollision speed dots <$> gameWalls)

-- | reset paddle velocity when the mouse stops
resetPaddleVel ::
     Game -- ^ current game state
  -> Game -- ^ game updated
resetPaddleVel game =
  if mouseEvent game
    then game {mouseEvent = False}
    else game {paddle = (paddle game) {paddleVel = (0, 0)}}

-- | Verify if the game is over (ball outside the game)
isGameOver ::
     Game -- ^ current game state
  -> Game -- ^ updated game
isGameOver game
  | y < -(gameHeight / 2) = game {gameState = GameOver}
  | otherwise = game
  where
    (_, y) = ballLoc game
