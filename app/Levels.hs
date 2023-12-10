{-
Contains definitions for different levels, including block layouts and special properties.

Implementation:
- Define a list of levels, each with a unique arrangement of blocks.
- Functions to load and initialize a level's state.
-}

module Levels where

import Constants
import GameTypes

gridSpot :: Integer -> Integer -> (Float, Float)
gridSpot x y = ((fromInteger x) * blockWidthCFG, (fromInteger y) * blockHeightCFG)

level1 :: Level
level1 = Level {
    levelNumber = 1,
    levelName = "Ezpz",
    levelBlocks = [Block (gridSpot x y) blockWidthCFG blockHeightCFG 1 Green | x <- [-2..2], y <- [-3..3], abs x == abs y]
}

level2 :: Level
level2 = Level {
    levelNumber = 2,
    levelName = "Annoyers",
    levelBlocks = 
            [Block (gridSpot x y) blockWidthCFG blockHeightCFG 1   Green | x <- [-4..4], y <- [-4..4], x * x + y * y < 3*3 && x * x + y * y > 1] ++
            [Block (gridSpot x 6) blockWidthCFG blockHeightCFG (-1) Grey | x <- [-4..4], even $ x] ++ 
            [Block (gridSpot x (-6)) blockWidthCFG blockHeightCFG 3     Red | x <- [-3..3]] ++ 
            [Block (gridSpot x (-5)) blockWidthCFG blockHeightCFG 2  Yellow | x <- [-3..3]]
}

level3 :: Level
level3 = Level {
    levelNumber = 3,
    levelName = "Literally Impossible",
    levelBlocks = 
            [Block (gridSpot x y) blockWidthCFG blockHeightCFG str col | x <- [-4..4], y <- [-4..4], let str = if abs x == 4 || abs y == 4 then (-1) else 1, let col = if abs x == 4 || abs y == 4 then Grey else Green]
}

levels :: [Level]
levels = [level1, level2, level3]