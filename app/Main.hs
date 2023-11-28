{-
The entry point of the game. It will initialize the game window and start the game loop.

Implementation:
- Import necessary modules (We could use Graphics.Gloss for the UI, etc..).
- Define the main function to initialize the game state and window.
- Call the game loop function from the Gloss library.
-}

module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Constants
import GameTypes
import InputHandler
import GameLogic
import Render (drawGame)


defaultGame :: Game
defaultGame = Game
    { gameInputState = GameInputState HoldStill False
    -- In Gloss (0, 0) is the center of the screen
    , gamePaddle = Paddle { paddlePosition = (0, -windowHeight / 2 + 5 + paddleHeightCFG)
                          , paddleWidth = paddleWidthCFG
                          , paddleHeight = paddleHeightCFG}
    , gameBall = Ball { ballPosition = (7, 41)
                      , ballVelocity = (80, 120)
                      , ballRadius = ballRadiusCFG}
    , gameBlocks = [ Block (3, 14) 20 10 3 Red
                   , Block (24, 3) 20 10 2 Yellow
                   , Block (53, 53) 20 10 1 Green
                   , Block (73, 73) 20 10 (-1) Grey  -- Grey block with negative strength, indicating unbreakable
                   ]
    , gameState = Running
    }

main :: IO ()
main = play
    (InWindow "HaskellBlockBreaker" (floor windowWidth, floor windowHeight) (30, 30))
    black fps defaultGame drawGame handleEvent updateGame
