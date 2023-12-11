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
import Levels


defaultGame :: Game
defaultGame = Game
    { gameInputState = GameInputState HoldStill False
    -- In Gloss (0, 0) is the center of the screen
    , gamePaddle = Paddle { paddlePosition = (-40, -windowHeight / 2 + 5 + paddleHeightCFG)
                          , paddleWidth = paddleWidthCFG
                          , paddleHeight = paddleHeightCFG}
    , gameBall = Ball { ballPosition = (0, 15 + (-windowHeight / 2 + paddleHeightCFG + ballRadiusCFG))
                      , ballVelocity = (0, 0)
                      , ballRadius = ballRadiusCFG}
    , gameBlocks = levelBlocks (head levels)
    , gameState = Waiting
    , gameLives = 3  -- Starting number of lives
    , gameLevel = head levels
    , gameTimer = 90.0
    }

main :: IO ()
main = play
    (InWindow "HaskellBlockBreaker" (newWidth, newHeight) (30, 30))
    black fps defaultGame drawGame handleEvent updateGame
    where
      newWidth = 800
      newHeight = 600
