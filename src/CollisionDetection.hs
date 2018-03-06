module CollisionDetection
  ( wallsCollision
  , bricksCollision
  , CollisionSide (..)
  , WallCollisionType (..)
  ) where


  import GameBoard


  -- | If there a collision on which side it accured
  data CollisionSide =
    TopSide | BottomSide | LeftSide | RightSide
    deriving Show

  -- | If there a collision with a wall on which side it accured
  data WallCollisionType =
    TopWall | BottomWall | LeftWall | RightWall

  -- | Given position and radius of the ball, return nothing if there is
  --   no collision or the collisionSide
  wallsCollision :: Position                -- ^ ball position
                -> Radius                   -- ^ ball radius
                -> Width                    -- ^ game width
                -> Height                    -- ^ game height
                -> Maybe WallCollisionType  -- ^ collision with the walls?
  wallsCollision (x, y) radius width height
          | y + radius >=  height / 2 = Just TopWall
          | y - radius <= -height / 2 = Just BottomWall
          | x - radius <= -width / 2 = Just LeftWall
          | x + radius >=  width / 2 = Just RightWall
          | otherwise = Nothing

  -- | Given position and radius of the ball, return nothing if there is
  --   no collision or the collisionSide
  bricksCollision :: Position      -- ^ ball position
                 -> Radius        -- ^ ball radius
                 -> [Brick]         -- ^ bricks List
                 -> (Maybe CollisionSide, [Brick])   -- ^ CollisionSide
  bricksCollision ballL ballR bricks = go ballL ballR bricks (Nothing,[])
            where go :: Position -> Radius -> [Brick]
                     -> (Maybe CollisionSide, [Brick])
                     -> (Maybe CollisionSide, [ Brick ])
                  go _ _ [] resp = resp
                  go ballL ballR (b:bs) (col, brickLts) =
                      case collision of
                        Nothing   -> go ballL ballR bs (col, b:brickLts)
                        _ -> (collision, brickLts ++ bs)
                      where
                        collision = rectangleCircleCollision ballL ballR
                                (brickX - brickWidth / 2, brickY - brickHeight / 2)
                                  brickWidth brickHeight
                        (brickX, brickY) = brickLoc b


  -- | Given position and raidus of the ball return nothing if there is
  --   no collision or the collisionSide
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
