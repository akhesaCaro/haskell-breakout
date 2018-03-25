module Maths
  ( intersecTime
  , dot
  ) where

-- aliases
type Vector = (Float, Float)
type SquareMatrix = (Vector, Vector)
type Point = (Float, Float)

-- | calculate the matrix multiplication between a Vector and a SquareMatrix
matrixMultiplication :: Vector        -- ^ vector
                     -> SquareMatrix  -- ^ square matrix to multiplicate with
                     -> Vector        -- ^ result qas a vector
matrixMultiplication (vx, vy) ((a, b), (c, d)) = ((vx * a + vx * b),(vy * c + vy * d))

-- | Calculate the dot product of two vectors
dot :: Vector   -- ^ first vector called va
    -> Vector   -- ^ second vector called vb
    -> Float    -- ^ Scalar dot product
dot (ax, ay) (bx, by) = (ax * bx) + (ay * by)

-- | Calculate the determinant of a vector
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

-- | Return, if it exists, the intersection time when the 2 segments defined
--   by a starting point and a direction vector collide.
intersecTime :: Vector       -- ^ vector v
             -> Point        -- ^ vector v starting point
             -> Vector       -- ^ vector ab
             -> Point        -- ^ vector ab starting point (point a)
             -> Maybe Float  -- ^ collision point
intersecTime (vx, vy) (vx0, vy0) (abx, aby) (ax0, ay0) = clamp cram
      where cram = cramer ((abx, aby), (-vx, -vy)) (vx0 - ax0, vy0 - ay0)

-- | return the point if all value are between the range 1 and 0
clamp :: Point        -- ^ Point to clap
      -> Maybe Float  -- ^ result
clamp (x, y) = let r = (,) <$> inRange x <*> inRange y
        in case r of
          Nothing -> Nothing
          Just (t1, t2) -> Just t1
      where inRange a | a > 1     = Nothing
                      | a < 0     = Nothing
                      | otherwise = Just a
