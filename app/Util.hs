{-
Utility functions that are used in various parts of the project.

Implementation:
General-purpose functions like distance calculation, angle conversions, etc..
-}

module Util where

-- Get hypotenuse / distance between two points
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ ((x1 - x2) ** 2) + ((y1 - y2) ** 2)

-- Get angle of vector. Range: -pi/2  to  3pi/2
vectorAngle :: (Floating a, Ord a) => (a, a) -> a
vectorAngle (0, y)
    | y > 0 = pi * 0.5
    | y < 0 = pi * 1.5
    | otherwise = 0
vectorAngle (x, 0)
    | x > 0 = 0
    | x < 0 = pi
vectorAngle (x, y)
    | x > 0 = atan (y / x)
    | x < 0 = atan (y / x) + pi
    | otherwise = 0
