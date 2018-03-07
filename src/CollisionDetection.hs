module CollisionDetection
  ( wallsCollision
  , bricksCollision
  , paddleCollision
  , CollisionSide (..)
  ) where


import GameBoard

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
