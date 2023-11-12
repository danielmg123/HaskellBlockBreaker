{-
Handles the rendering of the game state to the screen.

Implementation:
- Function to convert the game state into a picture (using Gloss).
- Separate functions to draw the paddle, ball, and blocks.
-}

module Render where

import Graphics.Gloss
import GameTypes

-- Draws a box
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

drawPaddle :: Paddle -> Picture
drawPaddle (Paddle (x, y) width height) =
    Color red (box x y width height)

drawBall :: Ball -> Picture
drawBall (Ball (x, y) _ r) = Translate x y (Color blue (circleSolid r))

drawBlock :: Block -> Picture
drawBlock (Block (x, y) width height strength) =
    Color (bright green) (box x y width height)

drawBlocks :: [Block] -> [Picture] -> Picture
drawBlocks [] ps = Pictures ps
drawBlocks (b:bs) ps = drawBlocks bs (drawBlock b : ps)

drawGame :: Game -> Picture
drawGame (Game paddle ball blocks state) =
    Pictures [
        drawPaddle paddle,
        drawBall ball,
        drawBlocks blocks []]