{-
Contains the core game logic, such as movement rules, collision detection, and game state updates.

Implementation:
- Functions for updating the game state each frame.
- Collision detection logic between the ball, blocks, and paddle.
- Scoring and level progression logic.
-}

module GameLogic where
import GameTypes
import Constants

-- Updates the game state
updateGame :: Float -> Game -> Game
updateGame delta game = game
    { gameBall = updatedBall
    , gamePaddle = updatePaddle delta (paddleMovement (gameInputState game)) paddle
    , gameBlocks = updatedBlocks
    }
  where
    ball = gameBall game
    paddle = gamePaddle game
    (updatedBlocks, updatedBall) = updateBlocksAndBall delta game

-- Function to update blocks and ball
updateBlocksAndBall :: Float -> Game -> ([Block], Ball)
updateBlocksAndBall delta game = foldr processBlock ([], updatedBallAfterPaddle) (gameBlocks game)
  where
    updatedBallAfterPaddle = updateBall delta (bounceOffPaddle delta (gameBall game) (gamePaddle game))
    processBlock block (blocks, ball) =
        if colType /= CollideNone
        then (hitBlock block ++ blocks, reflectedBall)
        else (block : blocks, ball)
            where
                colType = detectCollision ball block
                reflectedBall = reflectBall ball colType

    hitBlock :: Block -> [Block]
    hitBlock (Block _ _ _ 1 _) = []
    hitBlock block@(Block _ _ _ _ Grey) = [block]
    hitBlock block = [block { blockStrength = blockStrength block - 1
                           , blockColor = updateBlockColor (blockColor block)}]

    reflectBall :: Ball -> CollisionType -> Ball
    reflectBall ball CollideHorizontal = ball { ballVelocity = (-(fst (ballVelocity ball)), snd (ballVelocity ball)) }
    reflectBall ball CollideVertical = ball { ballVelocity = (fst (ballVelocity ball), -(snd (ballVelocity ball))) }
    -- Corner collisions...?
    reflectBall ball _ = ball

    updateBlockColor :: BlockColor -> BlockColor
    updateBlockColor Red = Yellow
    updateBlockColor Yellow = Green
    updateBlockColor Green = Green
    updateBlockColor Grey = Grey  -- Grey blocks don't change color


data CollisionType = CollideVertical | CollideHorizontal | CollideCorner | CollideNone
    deriving (Eq)

-- Check collision between ball and block
detectCollision :: Ball -> Block -> CollisionType
detectCollision (Ball (bx, by) _ radius) (Block (x, y) width height _ _)
    | bx + radius < x || bx - radius > x2 ||
      by + radius < y || by - radius > y2   = CollideNone
    | (bx > x && bx < x2) &&
      (by + radius > y || by - radius < y2) = CollideVertical
    | (by > y && by < y2) &&
      (bx + radius > x || bx - radius < x2) = CollideHorizontal
    | otherwise                             = CollideNone
        where
            x2 = x + width
            y2 = y + height

-- Negates velocity if ball is hitting a boundary
-- It needs to be changed. It only bounces in a square around the center
bounceOffBoundaries :: Float -> Ball -> Ball
bounceOffBoundaries border ball@(Ball (x, y) (vx, vy) radius)
    | x >= border || x <= -border = ball {ballVelocity = (-vx, vy)}
    | y >= border || y <= -border = ball {ballVelocity = (vx, -vy)}
    | otherwise = ball

bounceOffPaddle :: Float -> Ball -> Paddle -> Ball
bounceOffPaddle delta ball paddle
    | bx > px - pmx && bx < px - pmx + pw && by > py && by < py + ph = 
        ball {ballVelocity = ((-50), -(snd bv))}
    | bx > px + pmx && bx < px + pmx + pw && by > py && by < py  + ph =
        ball {ballVelocity = ((50), -(snd bv))}
    | otherwise = ball
        where
            bx = (fst $ ballPosition ball) - (ballRadius ball)
            by = (snd $ ballPosition ball) - (ballRadius ball)
            bv = ballVelocity ball
            px = fst $ paddlePosition paddle
            py = snd $ paddlePosition paddle
            pw = paddleWidth paddle
            ph = paddleHeight paddle
            pmx = pw/2

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
updatePaddle delta movement paddle = 
    let
        halfPaddleWidth = paddleWidth paddle / 2
        leftLimit = -halfWidth - 25
        rightLimit = halfWidth - 55
        newX = case movement of
            MoveRight -> min rightLimit (x + 5)
            MoveLeft  -> max leftLimit (x - 5)
            _         -> x
    in
        paddle { paddlePosition = (newX, y) }
    where
        x = fst $ paddlePosition paddle
        y = snd $ paddlePosition paddle
        halfWidth = (windowWidth / 2)  -- As defined in drawBorders


