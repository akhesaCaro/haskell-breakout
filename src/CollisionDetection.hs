module CollisionDetection
  ( wallsCollision
  , bricksCollision
  , rectangleDotCollision
  , rectanglesDotCollision
  , CollisionSide (..)
  ,
  ) where


import GameBoard
import Maths

import Data.Maybe
import Debug.Trace

-- | If there a collision on which side it accured
data CollisionSide =
      TopSide | BottomSide | LeftSide | RightSide
      deriving Show

-- | Given position and radius of the ball, return nothing if there is
--   no collision or the collisionSide.
wallsCollision :: Position              -- ^ ball position
               -> Radius                -- ^ ball radius
               -> Width                 -- ^ game width
               -> Height                -- ^ game height
               -> Maybe CollisionSide   -- ^ collision with the walls?
wallsCollision (x, y) radius width height
      -- Collision on the bottom side of top wall
      | y + radius >=  height / 2 = Just BottomSide
      -- Collision on the top side of bottom wall
      | y - radius <= -height / 2 = Just TopSide
      -- Collision on the right side of left wall
      | x - radius <= -width / 2 = Just RightSide
      -- Collision on the left side of right wall
      | x + radius >=  width / 2 = Just LeftSide
      | otherwise = Nothing

-- | Given position and radius of the ball, return nothing if there is
--   no collision or the collisionSide
bricksCollision :: Position                         -- ^ ball position
                -> Radius                           -- ^ ball radius
                -> [Brick]                          -- ^ bricks List
                -> (Maybe CollisionSide, [Brick])   -- ^ CollisionSide
bricksCollision ballL ballR bricks = go ballL ballR bricks (Nothing,[])
      where go :: Position
               -> Radius
               -> [Brick]
               -> (Maybe CollisionSide, [Brick])
               -> (Maybe CollisionSide, [ Brick ])
            go _ _ [] resp = resp
            go ballL ballR (b:bs) (col, brickLts) = case collision of
              Nothing   -> go ballL ballR bs (col, b:brickLts)
              _ -> (collision, brickLts ++ bs)
              where
                (brickX, brickY) = brickLoc b
                collision = rectangleCircleCollision ballL ballR
                  (brickX - brickWidth / 2, brickY - brickHeight / 2)
                    brickWidth brickHeight

-- | Given position and radius of the ball, return whether a
--   collision occured on the paddle
paddleCollision :: Position      -- ^ ball position
                -> Radius        -- ^ ball radius
                -> Position      -- ^ paddle position
                -> Width         -- ^ paddle width
                -> Height        -- ^ paddle height
                -> Maybe CollisionSide -- ^ collision side if collision
paddleCollision = rectangleCircleCollision

-- | Calculate the velocity vector of the collision if there is a collision
--   with the ball and any of rectangle in the list
rectanglesDotCollision :: Position        -- ^ Ball center position
                       -> Velocity        -- ^ Ball velocity
                       -> [Rectangle]     -- ^ list of rectangle (to check for the collision)
                       -> Velocity  -- ^ velocity if a collision with one of the rectangme
rectanglesDotCollision p vel rectangles = case velocityLst of
                              []      -> vel
                              (v:vs)  -> v
                              where
                              velocityLst :: [Velocity]
                              velocityLst = catMaybes $ fmap (rectangleDotCollision p vel) rectangles

-- | Calculate with the scalar dot product on which side there is
--   a collision or nothing if there is no collision
rectangleDotCollision :: Position     -- ^ dot position
                      -> Velocity     -- ^ dot velocity
                      -> Rectangle    -- ^ Rentangle
                      -> Maybe Velocity  -- ^ new circle velocity
rectangleDotCollision ballDot ballVelocity@(vx, vy)
  ((rectX, rectY), rectW, rectH)
      | scalarProductTopSide < 0 &&
              isJust (intersecPoint ballDot ballVelocity (rectW, 0) cornerTopLeft)
                = Just (vx, -vy)
      | scalarProductBottomSide < 0 &&
              isJust (intersecPoint ballDot ballVelocity (rectW, 0) cornerBottomLeft)
                = Just (vx, -vy)
      | scalarProductLeftSide < 0 &&
              isJust (intersecPoint ballDot ballVelocity (0, -rectH) cornerTopLeft)
                = Just (-vx, vy)
      | scalarProductRightSide < 0 &&
              isJust (traceShowId $ intersecPoint ballDot ballVelocity (0, -rectH) cornerTopRight)
                = Just (-vx, vy)
      | otherwise = Nothing
      where
        scalarProductTopSide = ballVelocity `dot` (0, 1)
        scalarProductBottomSide = ballVelocity `dot` (0, -1)
        scalarProductLeftSide = ballVelocity `dot` (-1, 0)
        scalarProductRightSide = ballVelocity `dot` (1, 0)
        cornerTopLeft = (rectX - rectW / 2, rectY + rectH / 2)
        cornerTopRight = traceShowId $ (rectX + rectW / 2, rectY + rectH / 2)
        cornerBottomLeft = (rectX - rectW / 2, rectY - rectH / 2)


-- | Given position and raidus of the ball return nothing if there is
--   no collision or the collisionSide
rectangleCircleCollision :: Position    -- ^ ball position
                         -> Radius      -- ^ ball radius
                         -> Position    -- ^ rectangle bottom left position
                         -> Width       -- ^ rectangle width
                         -> Height           -- ^ rectangle Height
                         -> Maybe CollisionSide   -- ^ collision side if collision
rectangleCircleCollision (ballX, ballY) ballR
  (rectX, rectY) rectW rectH
      | not collision = Nothing
      -- somewhere lefter than left side of the rectangle
      | nearestX < rectX = Nothing
      -- somewhere righter than right side of the rectangle
      | nearestX > rectX + rectW = Nothing
      -- somewhere on top side of the rectangle
      | nearestY < rectY = Nothing
      -- somewhere under bottom side
      | nearestY > rectY + rectH = Nothing
      -- collision on Top
      | nearestY <= rectY + rectH = Just TopSide
      -- collision on Bottom
      | nearestY >= rectY = Just BottomSide
      -- collision on Left
      | nearestX <= rectX = Just LeftSide
      --collision on Right
      | nearestX >= rectX + rectW = Just RightSide
      where
        collision = (deltaX * deltaX + deltaY * deltaY) < (ballR * ballR)
        deltaX = ballX - nearestX
        deltaY = ballY - nearestY
        nearestX = max rectX (min ballX (rectX + rectW))
        nearestY = max rectY (min ballY (rectY + rectW))
