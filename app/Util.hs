{-
Utility functions that are used in various parts of the project.

Implementation:
General-purpose functions like distance calculation, angle conversions, etc..
-}

module Util where

-- Get hypotenuse / distance between two points
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ ((x1 - x2) ** 2) + ((y1 - y2) ** 2)

vectorAngle :: (Floating a, Ord a) => (a, a) -> a
vectorAngle (0, y)
    | y > 0 = 3.14159265 * 0.5
    | y < 0 = 3.14159265 * 1.5
    | otherwise = 0
vectorAngle (x, 0)
    | x > 0 = 0
    | x < 0 = 3.14159265
vectorAngle (x, y)
    | x > 0 = atan (y / x)
    | x < 0 = atan (y / x) + 3.14159265