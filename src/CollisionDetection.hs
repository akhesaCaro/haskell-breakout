module CollisionDetection where

  import GameBoard

  data WallCollisionType =
    TopWall | BottomWall | LeftWall | RightWall

  -- | Given position and radius of the ball, return whether
  --   a collision occurred on the wall.
  wallsCollision :: Position                -- ^ ball position
                -> Radius                   -- ^ ball radius
                -> Width                    -- ^ game width
                -> Height                    -- ^ game height
                -> Maybe WallCollisionType  -- ^ collision with the walls?
  wallsCollision (x, y) radius width height
          | y + radius >=  height / 2 = Just TopWall    -- ^ Top wall
          | y - radius <= -height / 2 = Just BottomWall -- ^ Bottom wall
          | x - radius <= -width / 2 = Just LeftWall    -- ^ Left wall
          | x + radius >=  width / 2 = Just RightWall   -- ^ Right wall
          | otherwise = Nothing

  -- | Given position and radius of the ball, return whether
  --   a collision occurred on the brick
  brickCollision :: Position      -- ^ ball position
                 -> Radius        -- ^ ball radius
                 -> Brick         -- ^ the brick
                 -> Maybe Brick   -- ^ the brick if it still exists
  brickCollision ballL ballR brick =
      if rectangleCircleCollision ballL ballR (rectX - brickWidth / 2, rectY - brickHeight / 2) brickWidth brickHeight
        then Nothing
        else Just brick
      where
        (rectX, rectY) = brickLoc brick


  -- | Given position and raidus of the ball, return whether
  --   a colision occurred on the rectangle
  rectangleCircleCollision :: Position    -- ^ ball position
                           -> Radius      -- ^ ball radius
                           -> Position    -- ^ rectangle bottom left position
                           -> Width       -- ^ rectangle width
                           -> Height      -- ^ rectangle Height
                           -> Bool        -- ^ collision?
  rectangleCircleCollision (ballX, ballY) ballR
    (rectX, rectY) rectW rectH =
      (deltaX * deltaX + deltaY * deltaY) < (ballR * ballR)
          where
            deltaX = ballX - max rectX (min ballX (rectX + rectW))
            deltaY = ballY - max rectY (min ballY (rectY + rectW))
