{-
Stores constant values used in the game (screen size, paddle dimensions, etc..).

Implementation:
Define constants that are used across multiple modules.
-}

module Constants where

windowWidth :: Float
windowWidth = 640

windowHeight :: Float
windowHeight = 480

fps :: Int
fps = 60

paddleWidthCFG :: Float
paddleWidthCFG = 80

paddleHeightCFG :: Float
paddleHeightCFG = 10

ballRadiusCFG :: Float
ballRadiusCFG = 6

blockWidthCFG :: Float
blockWidthCFG = 24

blockHeightCFG :: Float
blockHeightCFG = 18

-- How fast the ball speeds up in pixels per second per second
ballSpeedUp :: Float
ballSpeedUp = 1.8