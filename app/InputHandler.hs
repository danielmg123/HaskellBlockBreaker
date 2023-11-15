{-
Manages user input (keyboard/mouse) and updates the game state accordingly.

Implementation:
Function to handle key events and update the paddle's position.
Additional input handlers as needed for game control (pausing the game etc..).
-}

module InputHandler where

import GameTypes
import Graphics.Gloss.Interface.IO.Game

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (Char k) Down _ _ ) game
    | k == 'a' = game {gameInputState = ginput {paddleMovement = MoveLeft}}
    | k == 'd' = game {gameInputState = ginput {paddleMovement = MoveRight}}
    | otherwise = game
        where
            ginput = gameInputState game
            pinput = paddleMovement ginput
handleEvent (EventKey (Char k) Up _ _ ) game
    | k == 'a' = game {gameInputState = ginput {paddleMovement = HoldStill}}
    | k == 'd' = game {gameInputState = ginput {paddleMovement = HoldStill}}
    | otherwise = game
        where
            ginput = gameInputState game
            pinput = paddleMovement ginput
handleEvent _ game = game