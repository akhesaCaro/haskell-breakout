module Maths
  ( intersecPoint
  , dot
  ) where

-- aliases
type Vector = (Float, Float)
type SquareMatrix = (Vector, Vector)
type Point = (Float, Float)

-- | Calculate the dot product
dot :: Vector   -- ^ first vector called va
    -> Vector   -- ^ second vector called vb
    -> Float    -- ^ Scalar dot product
dot (ax, ay) (bx, by) = (ax * bx) + (ay * by)

-- | Calculate determinant of a vector
determinant :: SquareMatrix
            -> Float    -- ^ determinant
determinant ((ax, ay), (bx, by)) = ax * by - bx * ay

-- | Use the Cramer theorem to resolve a linear equation
cramer :: SquareMatrix      -- ^ base (inversible matrix)
       -> Vector            -- ^ vector
       -> Vector            -- ^ vector with unknown variables resolved
cramer ((v1x, v1y), (v2x, v2y)) (rx, ry) =
      ( determinant ((rx, ry), (v2x, v2y)) / det
      , determinant ((v1x, v1y), (rx, ry)) / det)
      where
        det = determinant ((v1x, v1y), (v2x, v2y))

-- | return the intersection point between 2 vectors, if exists
intersecPoint :: Vector       -- ^ vector v
              -> Point        -- ^ vector v starting point
              -> Vector       -- ^ vector ab
              -> Point        -- ^ vector ab starting poiny (point a)
              -> Maybe Point  -- ^ collision point
intersecPoint (vx, vy) (vx0, vy0) (abx, aby) (ax0, ay0) = clamp cram
      where cram = cramer ((abx, aby), (-vx, -vy)) (vx0 - ax0, vy0 - ay0)

-- | return the point if all value are between the range 1 and 0
clamp :: Point        -- ^ Point to clap
      -> Maybe Point
clamp (x, y) = let r = (,) <$> inRange x <*> inRange y
        in case r of
          Nothing -> Nothing
          Just _ -> r
      where inRange a | a > 1     = Nothing
                      | a < 0     = Nothing
                      | otherwise = Just a
