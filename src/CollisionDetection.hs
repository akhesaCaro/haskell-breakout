module CollisionDetection where

  import Debug.Trace
  import GameBoard

  data CollisionSide =
    TopSide | BottomSide | LeftSide | RightSide
    deriving Show

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
  bricksCollision :: Position      -- ^ ball position
                 -> Radius        -- ^ ball radius
                 -> [Brick]         -- ^ bricks List
                 -> (Maybe CollisionSide, [Brick])   -- ^ CollisionSide
  bricksCollision ballL ballR bricks = go ballL ballR bricks (Nothing,[])
            where go :: Position -> Radius -> [Brick] -> (Maybe CollisionSide, [Brick]) -> (Maybe CollisionSide, [ Brick ])
                  go _ _ [] resp = resp
                  go ballL ballR (b:bs) (col, brickLts) =
                      case collision of
                        Nothing   -> go ballL ballR bs (col, b:brickLts)
                        _ -> (collision, brickLts ++ bs)
                      where
                        collision = rectangleCircleCollision ballL ballR (brickX - brickWidth / 2, brickY - brickHeight / 2) brickWidth brickHeight
                        (brickX, brickY) = brickLoc b


  -- | Given position and raidus of the ball, return whether
  --   a colision occurred on the rectangle
  rectangleCircleCollision :: Position    -- ^ ball position
                           -> Radius      -- ^ ball radius
                           -> Position    -- ^ rectangle bottom left position
                           -> Width       -- ^ rectangle width
                           -> Height           -- ^ rectangle Height
                           -> Maybe CollisionSide   -- ^ collision side
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
