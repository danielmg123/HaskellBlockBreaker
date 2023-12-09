{-
Handles the rendering of the game state to the screen.

Implementation:
- Function to convert the game state into a picture (using Gloss).
- Separate functions to draw the paddle, ball, and blocks.
-}

module Render where

import Graphics.Gloss
import GameTypes
import Constants

-- Draws a box (x y width height)
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

drawPaddle :: Paddle -> Picture
drawPaddle (Paddle (x, y) width height) =
    Color orange $ box x y width height

drawBall :: Ball -> Picture
drawBall (Ball (x, y) _ r) = Translate x y $ Color blue $ circleSolid r

drawBlock :: Block -> Picture
drawBlock (Block (x, y) width height _ color) =
    Color (colorToRGB color) $ box x y width height

colorToRGB :: BlockColor -> Color
colorToRGB Red = red
colorToRGB Yellow = yellow
colorToRGB Green = green
colorToRGB Grey = greyN 0.5

drawBlocks :: [Block] -> Picture
drawBlocks blocks = Pictures $ map drawBlock blocks

drawGame :: Game -> Picture
drawGame (Game _ paddle ball blocks _) =
    Pictures [
        drawPaddle paddle,
        drawBall ball,
        drawBlocks blocks,
        drawBorders
    ]


-- Draws the game borders
drawBorders :: Picture
drawBorders = Color (greyN 0.5) $ Pictures
    [ line [(-halfWidth, halfHeight), (halfWidth, halfHeight)]   -- Top border
    -- , line [(-halfWidth, -halfHeight), (halfWidth, -halfHeight)] -- Bottom border
    , line [(-halfWidth, halfHeight), (-halfWidth, -halfHeight)] -- Left border
    , line [(halfWidth, halfHeight), (halfWidth, -halfHeight)]   -- Right border
    ]
  where
    halfWidth = (windowWidth / 2 + ballRadiusCFG) / 1.32  -- Adjust for the ball radius
    halfHeight = windowHeight / 2 + ballRadiusCFG


