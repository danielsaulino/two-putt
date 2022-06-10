module Game.Type where

-- | 'ReaderT' adds a read-only environment to the given monad
import Control.Monad.Trans.Reader (ReaderT(..)) 

-- | 'evalStateT' evaluates a state computation with the given initial
--    state and return the final value, discarding the final state.
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

-- | data type TileType that has several contructors
--   Hole that takes an (Int,Int) tuple with the X Y coordinates of the hole,
--   other contructors like SouthWest, South, East that also take (Int,Int) tuple which is
--   the direction vector that represents the the slope on the field
data TileType
    = Hole (Int, Int)
    | Southwest (Int, Int)
    | South (Int, Int)
    | Southeast (Int, Int)
    | West (Int, Int)
    | Flat (Int, Int)
    | East (Int, Int)
    | Northwest (Int, Int)
    | North (Int, Int)
    | Northeast (Int, Int)
    | Unknown
    deriving Show

-- | type Game
--   this is the "stack of monads" ReaderT, StateT and IO
type Game env state a = ReaderT env (StateT state IO) a

-- | 'evalStateT' evaluates a state computation with the given initial 
--    state and return the final value, discarding the final state. 
runGame :: env -> state -> Game env state a -> IO a
runGame env state action = evalStateT (runReaderT action env) state