{-
Contains the core game logic, such as movement rules, collision detection, and game state updates.

Implementation:
- Functions for updating the game state each frame.
- Collision detection logic between the ball, blocks, and paddle.
- Scoring and level progression logic.
-}

module GameLogic where
import GameTypes

-- Updates the game state
updateGame :: Float -> Game -> Game
updateGame delta game = game
    {
        gameBall = updateBall delta (bounceOffPaddle delta ball paddle),
        gamePaddle = updatePaddle delta (paddleMovement (gameInputState game)) paddle
    }
    where
        ball = gameBall game
        paddle = gamePaddle game

-- Negates velocity if ball is hitting a boundary
-- It needs to be changed. It only bounces in a square around the center
bounceOffBoundaries :: Float -> Ball -> Ball
bounceOffBoundaries border ball@(Ball (x, y) (vx, vy) radius)
    | x >= border || x <= -border = ball {ballVelocity = (-vx, vy)}
    | y >= border || y <= -border = ball {ballVelocity = (vx, -vy)}
    | otherwise = ball

bounceOffPaddle :: Float -> Ball -> Paddle -> Ball
bounceOffPaddle delta ball paddle
    | bx > px && bx < px + pw && by > py && by < py + ph =
        ball {ballVelocity = (fst bv, -(snd bv))}
    | otherwise = ball
        where
            bx = fst $ ballPosition ball
            by = snd $ ballPosition ball
            bv = ballVelocity ball
            px = fst $ paddlePosition paddle
            py = snd $ paddlePosition paddle
            pw = paddleWidth paddle
            ph = paddleHeight paddle

-- Updates the ball
updateBall :: Float -> Ball -> Ball
updateBall delta ball =
    updateBallPosition delta $
    bounceOffBoundaries 240 ball

-- Adds the velocity to the position of the ball
updateBallPosition :: Float -> Ball -> Ball
updateBallPosition delta ball = ball
    {ballPosition = (fst pos + (fst vel * delta), snd pos + (snd vel * delta))}
    where
        pos = ballPosition ball
        vel = ballVelocity ball

updatePaddle :: Float -> PaddleInputState -> Paddle -> Paddle
updatePaddle delta movement paddle
    | movement == MoveRight = paddle {paddlePosition = (x + 5, y)}
    | movement == MoveLeft = paddle {paddlePosition = (x - 5, y)}
    | otherwise = paddle
        where
            x = fst $ paddlePosition paddle
            y = snd $ paddlePosition paddle