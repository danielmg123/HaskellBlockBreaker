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
  , gameLives :: Int    -- The number of lives remaining
  , gameLevel :: Level
  , gameTimer :: Float
  }

-- The GameState type represents the different possible states of the game.
data GameState = Menu | Waiting | Running | NextLevel | Paused | LostLife | GameOver
  deriving (Eq, Show)

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

data BlockColor = Grey | Green | Yellow | Red deriving (Eq)

-- The Block type represents a single block that the ball can break.
data Block = Block
  { blockPosition :: Point  -- The position of the block
  , blockWidth :: Float     -- The width of the block
  , blockHeight :: Float    -- The height of the block
  , blockStrength :: Int    -- The strength of the block (hits required to break it)
  , blockColor :: BlockColor -- The color of the block
  }

-- The CollisionType represents how a collision between the ball and a block occured
data CollisionType = CollideVertical | CollideHorizontal | CollideNE | CollideNW | CollideSE | CollideSW | CollideNone
    deriving (Eq)

data Level = Level {
      levelNumber :: Int
    , levelName :: String
    , levelBlocks :: [Block]
}