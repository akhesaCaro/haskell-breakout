module CollisionDetection
  ( detectDotsCollision
  , Speed
  , CollisionSide (..)
  ) where

import Data.List (sortOn)
import GameBoard
import Maths

import Data.Maybe

-- | Collision side
data CollisionSide =
      LeftSide | TopSide | RightSide | BottomSide
      deriving Show

-- Aliases
type Speed = (Float, Float)

-- | Calculate if there a hit between the ball and one of the brick
--   the system the list of bricks updated and the new speed of the ball
bricksCollision :: Position   -- ^ dot position
                 -> Speed     -- ^ object speed (velocity * seconds since last update)
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
              collision
                    = rectangleDotCollision ballCenter ballSpeed
                      (brickLoc brick, brickWidth, brickHeight)

-- -- | Calculate the speed vector of the collision if there is a collision
-- --   with the dot and any of rectangle in the list
-- rectanglesDotCollision :: Position        -- ^ dot position
--                        -> Speed           -- ^ object speed
--                        -> [Rectangle]     -- ^ list of rectangle (to check for the collision)
--                        -> Speed           -- ^ new speed if a collision with one of the rectangle
-- rectanglesDotCollision p speed rectangles = case speedLst of
--                               -- no hits, no new speed
--                               []      -> speed
--                               (s:ss)  -> s
--                               where
--                               speedLst :: [Speed]
--                               speedLst = catMaybes $ fmap (rectangleDotCollision p speed) rectangles

--
-- rectanglesDotsCollision :: [Position        -- ^ dot position
--                        -> Speed           -- ^ object speed
--                        -> [Rectangle]     -- ^ list of rectangle (to check for the collision)
--                        -> Speed           -- ^ new speed if a collision with one of the rectangle
-- rectanglesDotsCollision p speed rectangles = case speedLst of
--                               -- no hits, no new speed
--                               []      -> speed
--                               (s:ss)  -> s
--                               where
--                               speedLst :: [Speed]
--                               speedLst = catMaybes $ fmap (rectangleDotCollision p speed) rectangles
--


-- | Calculate with the scalar dot product on which side there is
--   a collision or nothing if there is no collision
rectangleDotCollision :: Position        -- ^ dot position
                      -> Speed           -- ^ dot speed
                      -> Rectangle       -- ^ Rentangle
                      -> Maybe Speed     -- ^ new object speed
rectangleDotCollision dotPoint speed@(sx, sy)
  ((rectX, rectY), rectW, rectH)
      -- if scalar product < 0 means there is a collision, so we calculate de collision point
      | scalarProductTopSide < 0 &&
              isJust (intersecPoint speed dotPoint (rectW, 0) cornerTopLeft)
                = Just (sx, -sy)
      | scalarProductBottomSide < 0 &&
              isJust (intersecPoint speed dotPoint (rectW, 0) cornerBottomLeft)
                = Just (sx, -sy)
      | scalarProductLeftSide < 0 &&
              isJust (intersecPoint speed dotPoint (0, -rectH) cornerTopLeft)
                = Just (-sx, sy)
      | scalarProductRightSide < 0 &&
              isJust (intersecPoint speed dotPoint (0, -rectH) cornerTopRight)
                = Just (-sx, sy)
      | otherwise = Nothing
      where
        -- scalar product with normal vector of rectangle top side
        scalarProductTopSide = speed `dot` (0, 1)
        -- scalar product with normal vector of rectangle bottom side
        scalarProductBottomSide = speed `dot` (0, -1)
        -- scalar product with normal vector of rectangle left side
        scalarProductLeftSide = speed `dot` (-1, 0)
        -- scalar product with normal vector of rectangle right side
        scalarProductRightSide = speed `dot` (1, 0)
        -- rectangle corner top left point
        cornerTopLeft = (rectX - rectW / 2, rectY + rectH / 2)
        -- rectangle corner top right point
        cornerTopRight = (rectX + rectW / 2, rectY + rectH / 2)
        -- rectangle corner top bottom point
        cornerBottomLeft = (rectX - rectW / 2, rectY - rectH / 2)





detectDotsCollision :: Speed      -- ^ ball speed
                    -> [Position] -- ^ ball dots
                    -> Rectangle  -- ^ renctangle
                    -> Maybe (Float, CollisionSide) -- ^ time of collsion and collison side tuple
detectDotsCollision v positions rect =
  let mCollisions = map (\pos -> detectDotCollision v pos rect) positions
      -- Sort by ascending t
      collisions = sortOn fst . catMaybes $ mCollisions in
  case collisions of
    [] -> Nothing
    (x:xs) -> Just x


detectDotCollision :: Position    -- ^ dot position
                -> Speed       -- ^ dot speed
                -> Rectangle   -- ^ rectangle
                -> Maybe (Float, CollisionSide)  -- ^ smallest t
                  -- (when the collision appears) and collisionSide
detectDotCollision dotPoint speed ((rectX, rectY), rectW, rectH)
      = let sides =
             [ (scalarProductTopSide, (rectW,0), cornerTopLeft, TopSide)
             , (scalarProductBottomSide, (rectW,0), cornerBottomLeft, BottomSide)
             , (scalarProductLeftSide, (0,-rectH), cornerTopLeft, LeftSide)
             , (scalarProductRightSide, (0,-rectH), cornerTopRight, RightSide)
             ]
            f (dotP, _, _, _) | dotP >= 0 = Nothing
            f (_, dirVec, startPoint, side) = do
              t <- intersecTime dotPoint speed startPoint dirVec
              return (t, side)
            collisions = sortOn fst $ mapMaybe f sides in
        case collisions of
          (x:xs) -> Just x
          _ -> Nothing
      where
        -- scalar product with normal vector of rectangle top side
        scalarProductTopSide = speed `dot` (0, 1)
        -- scalar product with normal vector of rectangle bottom side
        scalarProductBottomSide = speed `dot` (0, -1)
        -- scalar product with normal vector of rectangle left side
        scalarProductLeftSide = speed `dot` (-1, 0)
        -- scalar product with normal vector of rectangle right side
        scalarProductRightSide = speed `dot` (1, 0)
        -- rectangle corner top left point
        cornerTopLeft = (rectX - rectW / 2, rectY + rectH / 2)
        -- rectangle corner top right point
        cornerTopRight = (rectX + rectW / 2, rectY + rectH / 2)
        -- rectangle corner top bottom point
        cornerBottomLeft = (rectX - rectW / 2, rectY - rectH / 2)
