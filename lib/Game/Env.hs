module Game.Env where

-- | data type Env
data Env = Env {
    size :: (Int, Int), -- ^ Size of the course 
    velocity :: Int,    -- ^ velocity of the ball
    maxPower :: Int,    -- ^ max power the ball can be hit with
    maxAccel :: Int     -- ^ max acceleration of the ball
}

-- | Function that sets the default environment for the game
defaultEnv :: Env
defaultEnv = Env {
    size = (10,20),      -- width and heigth of the course (9, 19),
    velocity = 1,        -- default velocity
    maxPower = 100,      -- default max stroke power
    maxAccel = 2         -- default max acceleration
}