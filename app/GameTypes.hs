{-
Defines data types and structures used throughout the game (Game State, Paddle, Ball, Block).

Implementation:
- Define data types like 'Game', 'Paddle', 'Ball', 'Block'.
- Include utility functions for initializing these types.
-}

module GameTypes where

-- The Point type represents a point in 2D space.
type Point = (Float, Float)

-- The Velocity type represents the velocity of an object in 2D space.
type Velocity = (Float, Float)

-- The Game type contains all the information about the current state of the game.
data Game = Game
  { gameInputState :: GameInputState  -- The input state
  , gamePaddle :: Paddle    -- The player's paddle
  , gameBall :: Ball       -- The ball in play
  , gameBlocks :: [Block]  -- The blocks remaining to be broken
  , gameState :: GameState -- The current state of the game
  }

-- The GameState type represents the different possible states of the game.
data GameState = Menu | Running | Paused | GameOver

-- The Paddle type represents the player's paddle.
data Paddle = Paddle
  { paddlePosition :: Point  -- The position of the paddle
  , paddleWidth :: Float     -- The width of the paddle
  , paddleHeight :: Float    -- The height of the paddle
  }

data GameInputState = GameInputState
  { paddleMovement :: PaddleInputState, -- How the paddle is moving
    paddleOtherAmbiguousThing :: Bool
  }

data PaddleInputState = MoveLeft | MoveRight | HoldStill deriving (Eq, Show)

-- The Ball type represents the ball.
data Ball = Ball
  { ballPosition :: Point     -- The position of the ball
  , ballVelocity :: Velocity  -- The velocity of the ball
  , ballRadius :: Float       -- The radius of the ball
  }

-- The Block type represents a single block that the ball can break.
data Block = Block
  { blockPosition :: Point  -- The position of the block
  , blockWidth :: Float     -- The width of the block
  , blockHeight :: Float    -- The height of the block
  , blockStrength :: Int    -- The strength of the block (hits required to break it)
  }
