module Game.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.Type (Game (..), TileType(..))
-- | The find function takes a predicate and a structure and returns the leftmost element of the structure matching the predicate, 
--   or Nothing if there is no such element.
import Data.List (find)

-- | data type Direction 
data Direction = Positive | Negative | Neutral
    deriving (Show, Eq)
-- | data type GameState  
data GameState = Menu | Game | LoadHole
    deriving (Show, Eq)
-- | data type PlayerState 
--   It has constructors for different player states while the player is playing (GameState = Game) like Aim, SetPower, Stroke and HoleOut
data PlayerState = Aim | SetPower | Stroke | HoleOut
    deriving (Show, Eq)

-- | data type State - this is the actual state of the game that will be updated with the next (nextInternal) function
data State = State {
    gameState :: GameState,                       -- ^ State of the game
    playerState :: PlayerState,                   -- ^ State of the player
    ballPosition :: (Int, Int),                   -- ^ position of the ball in X Y coords 
    ballDirection :: (Direction, Direction),      -- ^ ball direction 
    strokeDirection :: (Direction, Direction),    -- ^ stroke direction 
    strokePower :: (Int, Int),                    -- ^ stroke power
    strokeNumber :: Int,                          -- ^ number of strokes
    holeNumber :: Int,                            -- ^ hole number
    holePosition :: (Int, Int),                   -- ^ coordinates of the hole
    holeTiles :: [[TileType]],                    -- ^ List of list of TileType that contains the given hole elements (slope, the ball...)
    totalScore :: Int                             -- ^ Total score
}


-- | default valies for the State
defaultState :: State
defaultState = State {
    gameState = Menu,
    playerState = Stroke,
    ballPosition = (9, 5),
    ballDirection = (Negative, Negative),
    strokeDirection = (Neutral, Neutral),
    strokePower = (10, 10),
    strokeNumber = 0,
    holeNumber = 1,
    holePosition = (0, 0),
    holeTiles = [[]],
    totalScore = 0
}

-- mapFile :: String
-- mapFile = "55555655555555555555\n55555655555555555555\n55055655555555555555\n55555655555555555555\n22233355555555555555\n55555555555555555555\n22222222222222222222\n55555555555555555555\n55555555555555555555\n55555555555555555555\n"

-- | String that contains chars from '0' to '9'. Each char will be used to create the TileType list
--   that will represent the golf course with a hole and different (Direction, Direction) tuples
--   that model the different slopes on the field
mapFile :: String
mapFile = "55555955555555655555\n55555955555555655555\n55555355505555655555\n55555355555555655555\n88877755555555222222\n55555555555555555555\n88888888889999999999\n55555555555555555555\n55555555555555555555\n55555555555555555555\n"

-- | Auxiliary function to convert an Int to a Direction
intToDirection :: Int -> Direction
intToDirection (-1) = Negative
intToDirection 0 = Neutral
intToDirection 1 = Positive
intToDirection _ = error "Invalid direction"

-- | Auxiliary function to convert a Direction to an Int
directionToInt :: Direction -> Int
directionToInt Negative = (-1)
directionToInt Neutral = 0
directionToInt Positive = 1

-- | takes a TileType and converts it to a (Direction,Direction) tuple
tileToDirection :: TileType -> (Direction, Direction)
tileToDirection (Southwest _) = (Positive, Negative)
tileToDirection (South _) = (Positive, Neutral)
tileToDirection (Southeast _) = (Positive, Positive)
tileToDirection (East _) = (Neutral, Positive)
tileToDirection (Northeast _) = (Negative, Positive)
tileToDirection (North _) = (Negative, Neutral)
tileToDirection (Northwest _) = (Negative, Negative)
tileToDirection (West _) = (Neutral, Negative)
tileToDirection _ = (Neutral, Neutral)

-- | takes a Direction representing the ball direction
--   another Direction for the slope of the field
--   and Int for the power of the stroke
--   and returns a Direction
collideDirection :: Direction -> Direction -> Int -> Direction
collideDirection Positive Positive _ = Positive                                          -- ^ If ball dir is Positive and slope dir Positive the resulting dir is Positive
collideDirection Positive Negative power = if power == 0 then Neutral else Positive      -- ^ If ball dir is Positive and slope dir Negative, the resulting dir is Neutral when the stroke power is 0 else Positive
collideDirection Positive Neutral _ = Positive                                           -- ^ If ball dir is Positive and slope dir Neutral the resulting dir is Positive
collideDirection Negative Positive power = if power == 0 then Neutral else Negative      -- ^ If ball dir is Negative and slope dir Positive, the resulting dir is Neutral when the stroke power is 0 else Negative
collideDirection Negative Negative _ = Negative                                          -- ^ If ball dir is Negative and slope dir Negative the resulting dir is Negative
collideDirection Negative Neutral _ = Negative                                           -- ^ If ball dir is Negative and slope dir Neutral the resulting dir is Negative
collideDirection Neutral Positive _ = Positive                                           -- ^ If ball dir is Neutral and slope dir Positive the resulting dir is Positive
collideDirection Neutral Negative _ = Negative                                           -- ^ If ball dir is Neutral and slope dir Negative the resulting dir is Negative
collideDirection Neutral Neutral _ = Neutral                                             -- ^ If ball dir is Neutral and slope dir Neutral the resulting dir is Neutral

-- | Auxiliary function to calulate the Direction to the hole relative to the ball
--   takes a Int for the X or Y coordinate of the ball
--   takes a Int for the X or Y coordinate of the hole
getDirectionToHole :: Int -> Int -> Direction
getDirectionToHole point1 point2
    | point1 - point2 > 0 = Negative
    | point1 - point2 < 0 = Positive
    | otherwise = Neutral

-- | takes an (Int, Int) tuple representing the ball position
--   another (Int, Int) tuple representing the hole position
--   returns the (Direction, Direction) tuple representing the direction to the hole
getDirectionsToHole :: (Int, Int) -> (Int, Int) -> (Direction, Direction)
getDirectionsToHole (x1, y1) (x2, y2) =
    (getDirectionToHole x1 x2, getDirectionToHole y1 y2)

-- | calculates how the acceleration of the ball changes depending on the direction of the course's slope
--   if both are Positve or both Negative acc will increase by 1
--   if directions are contraries the acc will decrease by 1
--   for all other case there won't be a change in acc
calcAccel :: Direction -> Direction -> Int
calcAccel Positive Positive = 1
calcAccel Negative Negative = 1
calcAccel Positive Negative = -1
calcAccel Negative Positive = -1
calcAccel _ _ = 0

-- | takes a Char and an (Int, Int) tuple representing a position and returns a TileType
createTileType :: Char -> (Int, Int) -> TileType
createTileType '0' pos = Hole pos
createTileType '1' pos = Southwest pos
createTileType '2' pos = South pos
createTileType '3' pos = Southeast pos
createTileType '4' pos = West pos
createTileType '5' pos = Flat pos
createTileType '6' pos = East pos
createTileType '7' pos = Northwest pos
createTileType '8' pos = North pos
createTileType '9' pos = Northeast pos
createTileType _ _ = Unknown


-- | takes a String that contains the chars that represents one line of the course map
--   and Int (the line number)
--   returns a list of TileType for that line
createTileLines :: String -> Int -> [TileType]
createTileLines tileString lineNumber = zipWith (\i ch -> createTileType ch (lineNumber, i)) [0..] tileString --  zipWith generalises zip by zipping with the function given as the first argument, instead of a tupling function

-- | takes a list of String 
--   returns the list of list of TileType which will represent the entire course (like hole number 1 or hole number 2, etc.)
parseMap :: [String] -> [[TileType]]
parseMap tileFile = zipWith (\i str -> createTileLines str i) [0..] tileFile


-- | takes a TileType and returns a Bool
isHole :: TileType -> Bool
isHole (Hole _) = True        -- ^ returns True if the TileType is a Hole
isHole _ = False              -- ^ returns False for all TileTypes


-- | takes a (Int,Int) tuple representing a position
--   and a TileType
--   returns a Bool - True if the given position matches the TileType position
--   otherwise False    
isTilePosition :: (Int, Int) -> TileType -> Bool
isTilePosition pos (Hole pos') = pos == pos'
isTilePosition pos (Southwest pos') = pos == pos'
isTilePosition pos (South pos') = pos == pos'
isTilePosition pos (Southeast pos') = pos == pos'
isTilePosition pos (West pos') = pos == pos'
isTilePosition pos (Flat pos') = pos == pos'
isTilePosition pos (East pos') = pos == pos'
isTilePosition pos (Northwest pos') = pos == pos'
isTilePosition pos (North pos') = pos == pos'
isTilePosition pos (Northeast pos') = pos == pos'
isTilePosition _ _ = False


-- | takes and (Int, Int) tuple representing a position and a List of List of TileType
--   returns the TileType that for the given position or an error "Tile not found"
findTileByPosition :: (Int, Int) -> [[TileType]] -> TileType
findTileByPosition pos tiles =
    --  find takes a predicate that returns a Bool (True for the TileType that matches the given position)
    --  and a structure (the [[TileType]]). Concat is used to obtain the concatenation of all the elements of a container of lists.
    --  and returns the leftmost element of the structure matching the predicate, or Nothing if there is no such element. 
    case find (isTilePosition pos) $ concat tiles of  
        Nothing -> error "Tile not found"             --  returns an error "Tile not found" when there wasn't and TileType matching the given position
        Just tile -> tile                             --  returns the TileType that matches the given position

-- | takes a list of list of TileType and returns an (Int, Int) tuple representing the position of the hole
--   or an error "No hole found"
findHolePosition :: [[TileType]] -> (Int, Int)
findHolePosition tileLines =
    case find (isHole) $ concat tileLines of   --  find takes the function isHole that checks if the TileType is a Hole and returns the leftmost element of the list of TileType
        Nothing -> error "No hole found"       --  returns an error when the hole is not found
        Just (Hole pos) -> pos                 --  returns the postion of the Hole

-- | takes an (Int, Int) tuple (a given position) 
--   and a list of list of TileType
--   and returns the (Direction, Direction) tuple for that given position
tileDirectionByPosition :: (Int, Int) -> [[TileType]] -> (Direction, Direction)
tileDirectionByPosition pos tiles = tileToDirection $ findTileByPosition pos tiles


-- | takes 3 (Int, Int) tuples representing the ball, hole XY coords and stroke power on the X and Y axis
--   and returns the corresponding PlayerState
calcPlayerState :: (Int, Int) -> (Int, Int) -> (Int, Int) -> PlayerState
calcPlayerState (ballX, ballY) (holeX, holeY) (strokePowerX, strokePowerY)
    | ballX == holeX && ballY == holeY = HoleOut                   --  HoleOut if the ball XY coords match the hole XY coords
    | strokePowerX == 0 && strokePowerY == 0 = SetPower            --  SetPower when the stroke power is 0
    | otherwise = Stroke                                           --  Stroke otherwise


-- | function of type Game env st a = ReaderT env (StateT st IO) a / "Stack of Monads": ReaderT (StateT (IO)) 
--   gets the environment and the prevState and updates the State of the game
next :: Game Env State ()
next = do
    env <- ask                                 --  uses ask to get the environment
    prevState <- lift get                      --  lifts once since State is one level down and uses get to obtain the previous State
    lift (put $ nextInternal env prevState)    --  lifts once since State is one level down and puts the new State (updated by the nextInternal function) back into the monadic structure

-- | takes an Env and the previous State of the game
--   returns the new State of the game
nextInternal :: Env -> State -> State
nextInternal (Env (width, height) velocity maxPower maxAccel) prevState@(State
    prevGameState                  
    prevPlayerState
    (prevBallX, prevBallY)
    (prevXDir, prevYDir)
    strokeDirection
    (strokePowerX, strokePowerY)
    prevStrokeNumber
    prevHoleNumber
    holePosition
    holeTiles
    prevTotalScore) =
        case prevGameState of                   --   the GameState starts in Menu, the one will be LoadHole and then will be Game (where the player is actually playing)                                              
            Menu -> State {                                                               --  case for when prevGameState = Menu
                gameState = LoadHole,                                                     --  the new gameState is LoadHole
                playerState = Aim,                                                        --  the new playerState is Aim
                ballPosition = ballPosition defaultState,                                 --  the ball position will be the one from the default State
                ballDirection = ballDirection defaultState,                               --  the ball direction will be the one from the default State
                strokeDirection = strokeDirection,                                        --  direction of stroke
                strokePower = (strokePowerX, strokePowerY),                               --  stroke power in x and y coords
                strokeNumber = 1,                                                         --  first stroke 
                holeNumber = 1,                                                           --  hole number 1
                holePosition = holePosition,                                              --  hole position
                holeTiles = holeTiles,                                                    --  list of list of TileType representing the entire hole map
                totalScore = prevTotalScore                                               --  the total score is previous total score
            }
            Game -> case prevPlayerState of                                               --  case for when prevGameState = Game
                Aim -> undefined                                                          --  case for when prevPlayerState = Aim is undefined 
                SetPower -> State {                                                       --  case for when prevPlayerState = SetPower
                    gameState = prevGameState,                                            --  the gameState is the previous gameState
                    playerState = Stroke,                                                 --  the playerState is Stroke
                    ballPosition = (newX, newY),                                          --  new ball position
                    ballDirection = getDirectionsToHole (newX, newY) holePosition,        --  ball direction based on the new ball position and the hole position 
                    strokeDirection = getDirectionsToHole (newX, newY) holePosition,      --  stroke direction based on the new ball position and the hole position
                    strokePower = (10, 10),                                               --  stroke power set to (10,10)
                    strokeNumber = prevStrokeNumber + 1,                                  --  number of strokes is increased by 1 
                    holeNumber = prevHoleNumber,                                          --  the hole number is the previous hole number
                    holePosition = holePosition,                                          --  position of the hole
                    holeTiles = holeTiles,                                                --  list of list of TileType representing the entire hole (course)
                    totalScore = prevTotalScore                                           --  the total score is previous total score
                }
                Stroke -> State {                                                         --  case for when prevPlayerState = Stroke
                    gameState = prevGameState,                                            --  the new gameState is previous gameState
                    playerState = newPlayerState,                                         --  new PlayerState
                    ballPosition = (newX, newY),                                          --  new ball position
                    ballDirection = (newXDir, newYDir),                                   --  new ball direction
                    strokeDirection = strokeDirection,                                    --  direction of the stroke
                    strokePower = (newStrokePowerX, newStrokePowerY),                     --  new stroke power
                    strokeNumber = prevStrokeNumber,                                      --  the number of strokes is the previous stroke number
                    holeNumber = prevHoleNumber,                                          --  the hole number is the previous hole number
                    holePosition = holePosition,                                          --  position of the hole
                    holeTiles = holeTiles,                                                --  list of list of TileType representing the entire hole (course)
                    totalScore = if newPlayerState == HoleOut                             --  the new total score
                                    then prevTotalScore + prevStrokeNumber                --  is the previous plus the stroke number if the player made it to the hole
                                    else prevTotalScore                                   --  else the total score is previous total score
                }
                HoleOut -> State {                                                        --  case for when prevPlayerState = HoleOut
                    gameState = Menu,                                                     --  the new gameState is Menu
                    playerState = prevPlayerState,                                        --  playerState is the previous playerState
                    ballPosition = (newX, newY),                                          --  new ball position
                    ballDirection = (newXDir, newYDir),                                   --  new ball direction
                    strokeDirection = strokeDirection,                                    --  direction of the stroke
                    strokePower = (newStrokePowerX, newStrokePowerY),                     --  new stroke power
                    strokeNumber = prevStrokeNumber,                                      --  the number of strokes is the previous stroke number
                    holeNumber = prevHoleNumber,                                          --  the hole number is the previous hole number
                    holePosition = holePosition,                                          --  position of the hole
                    holeTiles = holeTiles,                                                --  list of list of TileType representing the entire hole (course)
                    totalScore = prevTotalScore                                           --  the total score is previous total score
                }
            LoadHole -> State {                                                           --  case for when prevGameState = LoadHole                         
                gameState = Game,                                                         --  the new gameState is Game
                playerState = Stroke,                                                     --  the new playerState is Stroke
                ballPosition = (prevBallX, prevBallY),                                    --  the ball position is the previous ball position
                ballDirection = (Negative, Neutral),                                      --  the ball direction is the initial one
                strokeDirection = (Negative, Neutral),                                    --  the stroke direction is the intial one
                strokePower = (10, 10),                                                   --  stroke power set to (10,10)
                strokeNumber = 1,                                                         --  first stroke for that hole
                holeNumber = prevHoleNumber,                                              --  the hole number is the previous hole number
                holePosition = newHolePosition,                                           --  new hole position
                holeTiles = newHoleTiles,                                                 --  list of list of TileType representing the entire hole (course)
                totalScore = totalScore defaultState                                      --  total score set to the default
            }
    where
        -- the list of list of TileType loaded from the map file that represents an entire hole
        newHoleTiles = parseMap (lines mapFile)   
        -- finds in the newHoleTiles the new hole position
        newHolePosition = findHolePosition newHoleTiles  
        -- calcPlayerState takes 3 (Int, Int) tuples representing the ball, hole XY coords and stroke power on the X and Y axis and returns the new PlayerState
        newPlayerState = calcPlayerState (prevBallX, prevBallY) newHolePosition (strokePowerX, strokePowerY) 
        -- based on the ball position and the list of list of TileType finds the tile for that position and return the (Direction, Direction) tuple representing the slope of the field
        (tileXDir, tileYDir) = tileToDirection $ findTileByPosition (prevBallX, prevBallY) holeTiles 
        -- calculates a new Direction  on X axis based on the directions of the ball, the slope of the field (tile) and the power stroke
        newXDirDetected = collideDirection prevXDir tileXDir strokePowerX
        -- calculates a new Direction on Y axis based on the directions of the ball, the slope of the field (tile) and the power stroke
        newYDirDetected = collideDirection prevYDir tileYDir strokePowerY
        -- calculates the new stroke power on X axis. 
        -- the stroke power will increased by 1 if the ball and tile directions are the same 
        -- the stroke power will decreased by 1 if the ball and tile directions are the opposite 
        -- oherwise unaffected.
        -- the result minus the velocity will give out the newStrokePowerX if it is > 0 otherwise it will be 0     
        newStrokePowerX = max ((strokePowerX + calcAccel prevXDir tileXDir) - velocity) 0
        -- calculates the new stroke power on Y axis. 
        -- the stroke power will increased by 1 if the ball and tile directions are the same 
        -- the stroke power will decreased by 1 if the ball and tile directions are the opposite 
        -- oherwise unaffected.
        -- the result minus the velocity will give out the newStrokePowerX if it is > 0 otherwise it will be 0        
        newStrokePowerY = max ((strokePowerY + calcAccel prevYDir tileYDir) - velocity) 0
        -- New X coordinate of the ball if there were no course limits
        -- the previous X coordinate of the ball plus (the result of the multiplication between an Int representing the newXDirDetected 
        -- and the min of the newStrokePowerX and velocity)
        newXUnbounded = prevBallX + directionToInt newXDirDetected * min newStrokePowerX velocity
        -- New Y coordinate of the ball if there were no course limits
        -- the previous Y coordinate of the ball plus (the result of the multiplication between an Int representing the newYDirDetected 
        -- and the min of the newStrokePowerY and velocity)
        newYUnbounded = prevBallY + directionToInt newYDirDetected * min newStrokePowerY velocity
        newX =                                                                            -- New position of the ball on X axis
            case prevXDir of                                                              -- based on the prev direction on X axis
                Neutral -> newXUnbounded                                                  -- case Neutral is newXUnbounded 
                Positive -> min newXUnbounded width                                       -- case Positive the min between newXUnbounded and the width of the course
                Negative -> max newXUnbounded 0                                           -- case Negative the max between newXUnbounded and 0
        newY =                                                                            -- New position of the ball on Y axis
            case prevYDir of                                                              -- based on the prev direction on Y axis
                Neutral -> newYUnbounded                                                  -- case Neutral is newYUnbounded
                Positive -> min newYUnbounded height                                      -- case Positive the min between newYUnbounded and the height of the course
                Negative -> max newYUnbounded 0                                           -- case Negative the max between newYUnbounded and 0
        newXDir =                                                                         -- ^ New direction of the ball on X axis
            case newXDirDetected of                                                       -- based on newXDirDetected
                Neutral -> Neutral                                                        -- case Neutral keeps Neutral
                Positive ->                                                               -- case Positive
                    if newXUnbounded > width                                              -- if newXUnbounded greater than the width
                    then Negative                                                         -- then Negative
                    else Positive                                                         -- else Positive 
                Negative ->                                                               -- case Negative
                    if newXUnbounded < 0                                                  -- if newXUnboundedl less than 0
                    then Positive                                                         -- then Positive
                    else Negative                                                         -- else Negative
        newYDir =                                                                         -- New direction of the ball on Y axis
            case newYDirDetected of                                                       -- based on newYDirDetected
                Neutral -> Neutral                                                        -- case Neutral keeps Neutral
                Positive ->                                                               -- case Positive
                    if newYUnbounded > height                                             -- if newYUnbounded greater than the heigth
                    then Negative                                                         -- then Negative
                    else Positive                                                         -- else Positive
                Negative ->                                                               -- case Negative
                    if newYUnbounded < 0                                                  -- if newYUnboundedl less than 0
                    then Positive                                                         -- then Positive
                    else Negative                                                         -- else Negative
