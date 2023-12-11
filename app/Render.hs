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
drawGame game
    | gameState game == Running = drawGameRunning game
    | gameState game == LostLife = drawGameLostLife game
    | gameState game == NextLevel = drawGameNextLevel game
    | gameState game == GameOver = drawGameGameOver game
    | otherwise = drawGameRunning game

drawGameRunning :: Game -> Picture
drawGameRunning game =
    Pictures [
        drawPaddle $ gamePaddle game,
        drawBall $ gameBall game,
        drawBlocks $ gameBlocks game,
        drawBorders,
        drawLives $ gameLives game,
        drawLevel $ gameLevel game
    ]

drawGameLostLife :: Game -> Picture
drawGameLostLife game = drawGameRunning game <> lostText 
    where
        lostText = Translate x y $ Scale 0.5 0.5 $ Color white $ Text "not good"
        x = -140
        y = 240 - (gameTimer game * 100)

drawGameNextLevel :: Game -> Picture
drawGameNextLevel game = drawGameRunning game <> nextText
    where
        nextText = Translate x y $ Scale 0.5 0.5 $ Color white $ Text "aww yeah"
        x = -140
        y = 240 - (gameTimer game * 100)

drawGameGameOver :: Game -> Picture
drawGameGameOver game = drawGameRunning game <> gameOverText
    where
        gameOverText = Translate x y $ Scale 0.5 0.5 $ Color white $ Text "Game Over"
        x = -170
        y = 240 - (gameTimer game * 100)

drawLives :: Int -> Picture
drawLives lives = Pictures [livesText, livesIcons]
  where
    livesText = Translate (40 + (windowWidth / 2 - 100)) (windowHeight / 2 - 30) . Scale 0.12 0.12 . Color white $ Text "Lives Left"
    livesIcons = Pictures $ take (lives - 1) $ zipWith (\n -> Translate (65 + (windowWidth / 2 - 100 + n * 20)) (windowHeight / 2 - 50)) [0..] (repeat (Color blue $ circleSolid 8))

drawLevel :: Level -> Picture
drawLevel level =
    Color white $
    Translate (windowWidth / (-2) - 40) (windowHeight / 2 - 30) $
    Pictures [
        Scale 0.12 0.12 $ Text ("Level: " ++ show (levelNumber level)),
        Translate 0 (-30) $ Scale 0.12 0.12 $ Text $ "\"" ++ levelName level ++ "\""
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


