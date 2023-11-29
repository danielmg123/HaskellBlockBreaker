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
import Util
import Graphics.Gloss.Data.Vector

-- Updates the game state
updateGame :: Float -> Game -> Game
updateGame delta game
    | ballHitsBottom = handleBallReset
    | otherwise = game
        { gameBall = updatedBall
        , gamePaddle = updatePaddle delta (paddleMovement (gameInputState game)) (gamePaddle game)
        , gameBlocks = updatedBlocks
        }
  where
    ball = gameBall game
    ballY = snd $ ballPosition ball
    ballRadius = ballRadiusCFG
    ballHitsBottom = ballY - ballRadius <= -windowHeight / 2

    (updatedBlocks, updatedBall) = updateBlocksAndBall delta game

    handleBallReset
        | gameLives game > 1 = game
            { gameBall = resetBall
            , gamePaddle = resetPaddle
            , gameLives = gameLives game - 1
            }
        | otherwise = resetGame

    resetBall = Ball 
        { ballPosition = (0, 15 + (-windowHeight / 2 + paddleHeightCFG + ballRadiusCFG))
        , ballVelocity = (0, 0)
        , ballRadius = ballRadiusCFG
        }

    resetPaddle = Paddle 
        { paddlePosition = (-40, -windowHeight / 2 + paddleHeightCFG)
        , paddleWidth = paddleWidthCFG
        , paddleHeight = paddleHeightCFG 
        }

    resetGame = game
        { gameBall = resetBall
        , gameBlocks = initialBlocks
        , gameLives = initialLives
        , gamePaddle = resetPaddle
        }

    initialBlocks = [Block (x * blockWidthCFG, y * blockHeightCFG) blockWidthCFG blockHeightCFG 1 Green | x <- [-4..4], y <- [-4..4], x * x + y * y < 3*3 && x * x + y * y > 1] ++
                    [Block (x * blockWidthCFG, 6 * blockHeightCFG) blockWidthCFG blockHeightCFG (-1) Grey | x <- [-4..4], even $ floor x] ++ 
                    [Block (x * blockWidthCFG, -6 * blockHeightCFG) blockWidthCFG blockHeightCFG 3 Red | x <- [-3..3]] ++ 
                    [Block (x * blockWidthCFG, -5 * blockHeightCFG) blockWidthCFG blockHeightCFG 2 Yellow | x <- [-3..3]]

    initialLives = 3  -- Set the initial number of lives here


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
                reflectedBall = reflectBall ball block colType

    hitBlock :: Block -> [Block]
    hitBlock (Block _ _ _ 1 _) = []
    hitBlock block@(Block _ _ _ _ Grey) = [block]
    hitBlock block = [block { blockStrength = blockStrength block - 1
                           , blockColor = updateBlockColor (blockColor block)}]

    reflectBall :: Ball -> Block -> CollisionType -> Ball
    reflectBall ball@(Ball _ (vx, vy) _) _ CollideHorizontal = ball { ballVelocity = (-vx, vy) }
    reflectBall ball@(Ball _ (vx, vy) _) _ CollideVertical = ball { ballVelocity = (vx, -vy) }
    reflectBall ball@(Ball (bx, by) v@(vx, vy) r) block colType = ball {ballVelocity = newVelocity}
        where
            (cx, cy) = cornerPoint block colType
            angleCorner = vectorAngle (bx - cx, by - cy)
            angleVelocity = vectorAngle (-vx, -vy)
            angleReflected = angleVelocity + (2 * (angleCorner - angleVelocity))
            newVelocity = mulSV (magV v) (cos angleReflected, sin angleReflected)
    reflectBall ball _ _ = ball

    -- Gets the corner point based on collision type
    cornerPoint :: Block -> CollisionType -> Point
    cornerPoint (Block (x, y) w h _ _) CollideNW = (x, y + h)
    cornerPoint (Block (x, y) w h _ _) CollideNE = (x + w, y + h)
    cornerPoint (Block (x, y) w h _ _) CollideSE = (x + w, y)
    cornerPoint (Block (x, y) w h _ _) _ = (x, y)

    updateBlockColor :: BlockColor -> BlockColor
    updateBlockColor Red = Yellow
    updateBlockColor Yellow = Green
    updateBlockColor Green = Green
    updateBlockColor Grey = Grey  -- Grey blocks don't change color

-- Check collision between ball and block
detectCollision :: Ball -> Block -> CollisionType
detectCollision (Ball (bx, by) _ radius) (Block (x, y) width height _ _)
    | bx + radius < x || bx - radius > x2 ||
      by + radius < y || by - radius > y2   = CollideNone
    | (bx > x && bx < x2) &&
      (by + radius > y || by - radius < y2) = CollideVertical
    | (by > y && by < y2) &&
      (bx + radius > x || bx - radius < x2) = CollideHorizontal
    | distance (bx, by) (x , y ) < radius   = CollideSW
    | distance (bx, by) (x2, y ) < radius   = CollideSE
    | distance (bx, by) (x , y2) < radius   = CollideNW
    | distance (bx, by) (x2, y2) < radius   = CollideNE
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
    | bx > px && bx < px + pw && by > py && by < py + ph =
        ball { ballVelocity = (newVx, -(snd bv)) }
    | otherwise = ball
        where
            bx = fst $ ballPosition ball
            by = (snd $ ballPosition ball) - (ballRadius ball)
            bv = ballVelocity ball
            px = fst $ paddlePosition paddle
            py = snd $ paddlePosition paddle
            pw = paddleWidth paddle
            ph = paddleHeight paddle

            -- bounce angle
            paddleMid = px + pw / 2
            distanceFromMid = bx - paddleMid
            maxBounceAngle = pi / 4  -- Increase num to increase angle
            ratio = distanceFromMid / (pw / 2)
            newVx = ratio * (ballSpeed * cos maxBounceAngle)
            ballSpeed = sqrt $ fst bv ** 2 + snd bv ** 2


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