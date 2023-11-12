{-
Handles the rendering of the game state to the screen.

Implementation:
- Function to convert the game state into a picture (using Gloss).
- Separate functions to draw the paddle, ball, and blocks.
-}

module Render where

import Graphics.Gloss
import GameTypes

drawPaddle :: Paddle -> Picture
-- drawPaddle _ = Color red (Polygon [(3, 3), (30, 3), (30, 20), (3, 20)])
drawPaddle (Paddle (x, y) w h) = Color red (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

drawBall :: Ball -> Picture
drawBall (Ball (x, y) _ r) = Translate x y (Color blue (circleSolid r))

drawGame :: Game -> Picture
drawGame (Game paddle ball blocks state) = Pictures [ drawPaddle paddle, drawBall ball]