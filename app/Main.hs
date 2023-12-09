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
    , gamePaddle = Paddle { paddlePosition = (-40, -windowHeight / 2 + 5 + paddleHeightCFG)
                          , paddleWidth = paddleWidthCFG
                          , paddleHeight = paddleHeightCFG}
    , gameBall = Ball { ballPosition = (0, 15 + (-windowHeight / 2 + paddleHeightCFG + ballRadiusCFG))
                      , ballVelocity = (0, 0)
                      , ballRadius = ballRadiusCFG}
    , gameBlocks =  [Block (x * blockWidthCFG, y * blockHeightCFG) blockWidthCFG blockHeightCFG 1 Green | x <- [-4..4], y <- [-4..4], x * x + y * y < 3*3 && x * x + y * y > 1] ++
                    [Block (x * blockWidthCFG, 6 * blockHeightCFG) blockWidthCFG blockHeightCFG (-1) Grey | x <- [-4..4], even (floor x :: Int)] ++ 
                    [Block (x * blockWidthCFG, -6 * blockHeightCFG) blockWidthCFG blockHeightCFG 3 Red | x <- [-3..3]] ++ 
                    [Block (x * blockWidthCFG, -5 * blockHeightCFG) blockWidthCFG blockHeightCFG 2 Yellow | x <- [-3..3]]
    , gameState = Running
    , gameLives = 3  -- Starting number of lives
    }

main :: IO ()
main = play
    (InWindow "HaskellBlockBreaker" (newWidth, newHeight) (30, 30))
    black fps defaultGame drawGame handleEvent updateGame
    where
      newWidth = 800
      newHeight = 600
