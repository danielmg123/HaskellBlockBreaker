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
paddleHeightCFG = 20

ballRadiusCFG :: Float
ballRadiusCFG = 12