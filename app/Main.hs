module Main where

import Game 
    ( Game,
      Env (..),
      State (..),
      defaultEnv,
      defaultState,
      renderIO,
      next,
      runGame )

-- | to create a delay on the computation 
import Control.Concurrent (threadDelay)
-- | To unwrap Monad Transformers ("lift" from the "Monad Stack")
import Control.Monad.Trans.Class (lift) 

-- | type Game env st a = ReaderT env (StateT st IO) a / "Stack of Monads": ReaderT (StateT (IO))
run :: Game Env State ()
run = do
  renderIO                              -- | renders the game
  next                                  -- | updates the state
  lift $ lift $ threadDelay 1000000     -- | As threadDelay :: Int -> IO() we have to "lift" it twice to the ReaderT level
  run                                   -- | Calls itself into an infinite loop

-- | Calls the runGame function with the default values
main :: IO ()
main = runGame defaultEnv defaultState run
