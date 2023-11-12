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
import Render (drawGame)

defaultGame :: Game
defaultGame = Game
    (Paddle (0, -windowHeight / 2 + 5 + paddleHeightCFG ) paddleWidthCFG paddleHeightCFG)
    (Ball (0, 0) (0, 0) ballRadiusCFG)
    []
    Running

placeholderInput :: Event -> Game -> Game
placeholderInput _ game = game

placeholderUpdate :: Float -> Game -> Game
placeholderUpdate _ game = game

main :: IO ()
main = play
    (InWindow "HaskellBlockBreaker" (floor windowWidth, floor windowHeight) (30, 30))
    black fps defaultGame drawGame placeholderInput placeholderUpdate
