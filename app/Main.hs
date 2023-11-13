{-
The entry point of the game. It will initialize the game window and start the game loop.

Implementation:
- Import necessary modules (We could use Graphics.Gloss for the UI, etc..).
- Define the main function to initialize the game state and window.
- Call the game loop function from the Gloss library.
-}

module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)
import Constants
import GameTypes
import GameLogic
import Render (drawGame)

defaultGame :: Game
defaultGame = Game
    -- In Gloss (0, 0) is the center of the screen
    (Paddle {
        paddlePosition = (0, -windowHeight / 2 + 5 + paddleHeightCFG)
        , paddleWidth = paddleWidthCFG
        , paddleHeight = paddleHeightCFG})
    (Ball {
        ballPosition = (7, 41)
        , ballVelocity = (63, 81)
        , ballRadius = ballRadiusCFG})
    [Block (3, 14) 20 10 1, Block (24, 3) 20 10 1, Block (53, 53) 20 10 1, Block (73, 73) 20 10 1]
    Running

placeholderInput :: Event -> Game -> Game
placeholderInput _ game = game

main :: IO ()
main = play
    (InWindow "HaskellBlockBreaker" (floor windowWidth, floor windowHeight) (30, 30))
    black fps defaultGame drawGame placeholderInput updateGame
