{-
Utility functions that are used in various parts of the project.

Implementation:
General-purpose functions like distance calculation, angle conversions, etc..
-}

-- Get hypotenuse / distance between two points
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ ((x1 - x2) ** 2) + ((y1 - y2) ** 2)