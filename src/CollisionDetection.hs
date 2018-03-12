module CollisionDetection
  ( bricksCollision
  , rectanglesDotCollision
  ) where

import GameBoard
import Maths

import Data.Maybe


-- aliases
type Speed = (Float, Float)

-- | Calculate if there a hit between the ball and one of the brick
--   the system the list of bricks updated and the new speed of the ball
bricksCollision :: Position  -- ^ ball position
                 -> Speed     -- ^ ball speed (velocity * seconds since last update)
                 -> [Brick]   -- ^ list of bricks
                 -> (Maybe Speed, [Brick])
bricksCollision ballCenter ballSpeed bricks = go ballCenter ballSpeed bricks (Nothing, [])
      where go :: Position
               -> Speed
               -> [Brick]
               -> (Maybe Speed, [Brick])
               -> (Maybe Speed, [Brick])
            go _ _ [] resp = resp
            go ballCenter ballSpeed (brick:bs) (speed, brickLts) = case collision of
              Nothing -> go ballCenter ballSpeed bs (speed, brick:brickLts)
              _ -> (collision, brickLts ++ bs)
              where
              collision = rectangleDotCollision ballCenter ballSpeed (brickLoc brick, brickWidth, brickHeight)

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
      -- if scalar product < 0 means there is a collision, so we calculate de collision point
      | scalarProductTopSide < 0 &&
              isJust (intersecPoint ballVelocity ballDot (rectW, 0) cornerTopLeft)
                = Just (vx, -vy)
      | scalarProductBottomSide < 0 &&
              isJust (intersecPoint ballVelocity ballDot (rectW, 0) cornerBottomLeft)
                = Just (vx, -vy)
      | scalarProductLeftSide < 0 &&
              isJust (intersecPoint ballVelocity ballDot (0, -rectH) cornerTopLeft)
                = Just (-vx, vy)
      | scalarProductRightSide < 0 &&
              isJust (intersecPoint ballVelocity ballDot (0, -rectH) cornerTopRight)
                = Just (-vx, vy)
      | otherwise = Nothing
      where
        -- scalar product with normal vector of rectangle top side
        scalarProductTopSide = ballVelocity `dot` (0, 1)
        -- scalar product with normal vector of rectangle bottom side
        scalarProductBottomSide = ballVelocity `dot` (0, -1)
        -- scalar product with normal vector of rectangle left side
        scalarProductLeftSide = ballVelocity `dot` (-1, 0)
        -- scalar product with normal vector of rectangle right side
        scalarProductRightSide = ballVelocity `dot` (1, 0)
        -- rectangle corner top left point
        cornerTopLeft = (rectX - rectW / 2, rectY + rectH / 2)
        -- rectangle corner top right point
        cornerTopRight = (rectX + rectW / 2, rectY + rectH / 2)
        -- rectangle corner top bottom point
        cornerBottomLeft = (rectX - rectW / 2, rectY - rectH / 2)
