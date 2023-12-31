{-
Manages user input (keyboard/mouse) and updates the game state accordingly.

Implementation:
Function to handle key events and update the paddle's position.
Additional input handlers as needed for game control (pausing the game etc..).
-}

module InputHandler where

import GameTypes
import Graphics.Gloss.Interface.IO.Game

handleEvent  :: Event -> Game -> Game
handleEvent event game
    | gameState game == Waiting = handleEventsWaiting event game
    | gameState game == Running = handleEventsRunning event game
    | otherwise = game

handleEventsWaiting :: Event -> Game -> Game
handleEventsWaiting (EventKey (Char k) Down _ _ ) game
    | k == 'a' || k == 'd' = startBallMovement
    | otherwise = game
        where
            startBallMovement =
                if fst (ballVelocity (gameBall game)) == 0 && snd (ballVelocity (gameBall game)) == 0
                then game {gameBall = (gameBall game) {ballVelocity = (0, 200)}, gameState = Running}
                else game
handleEventsWaiting _ game = game

handleEventsRunning :: Event -> Game -> Game
handleEventsRunning (EventKey (Char k) Down _ _ ) game
    | k == 'a' || k == 'd' = movePaddle
    | otherwise = game
        where
            movePaddle = game {gameInputState = ginput {paddleMovement = movement}}
                where
                    ginput = gameInputState game
                    movement = if k == 'a' then MoveLeft else MoveRight
                
handleEventsRunning (EventKey (Char k) Up _ _ ) game
    | k == 'a' = game {gameInputState = ginput {paddleMovement = HoldStill}}
    | k == 'd' = game {gameInputState = ginput {paddleMovement = HoldStill}}
    | otherwise = game
        where
            ginput = gameInputState game
handleEventsRunning _ game = game