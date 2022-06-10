module Game.Render where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.State (State (..), GameState (..), PlayerState (..), tileDirectionByPosition)
import Game.Type (Game, TileType(..))

-- | auxiliary function that takes an (Int,Int) tuple with the X Y coordinates of the tile
--   an (Int,Int) tuple with the X Y coordinates of the ball
--   and a character 
--   renders and '*' if the tile coordinates match the ball coordinates
--   otherwise the character
renderTileCharacter :: (Int, Int) -> (Int, Int) -> Char -> Char
renderTileCharacter (tileX, tileY) (ballX, ballY) ch
    | tileX == ballX && tileY == ballY = '*'
    | otherwise = ch

-- | takes a TileType with the inclination vector representing the slope on the field
--   and the (Int,Int) tuple with the coordinates of the ball
--   gives out an 'O' to render the hole or an arrow representing the slope direction
--   or '*' representing the ball if the balls coords match the tyle coords
renderTile :: TileType -> (Int, Int) -> Char
renderTile (Hole tilePos) ballPos = renderTileCharacter tilePos ballPos 'O'
renderTile (Southwest tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2199'
renderTile (South tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2193'
renderTile (Southeast tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2198'
renderTile (West tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2190'
renderTile (Flat tilePos) ballPos = renderTileCharacter tilePos ballPos ' '
renderTile (East tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2192'
renderTile (Northwest tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2196'
renderTile (North tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2191'
renderTile (Northeast tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2197'
renderTile Unknown _ = error "Unknown tile type"

-- | Takes a list of TileType and an (Int,Int) tuple with the ball coords
--   and returns a String representing a line of the golf course
--   it maps the renderTile function over the list of TileType
renderTileLines :: [TileType] -> (Int, Int) -> String
renderTileLines xs ballPos = "|" ++ map (`renderTile` ballPos) xs ++ "|\n"

-- | Takes a list of list of TileType and (Int,Int) tuple with the ball coords
--   and returns a String
--   it maps the renderTileLines funcion over the elements list of list of TileType 
--   and concatenas its results
renderTiles :: [[TileType]] -> (Int, Int) -> String
renderTiles xs ballPos = concatMap (`renderTileLines` ballPos) xs


-- | auxiliary function that takes and (Int, Int) tuple representing a position with x y coordinates
--   and returns a String 
--   it renders a position
renderPosition :: (Int, Int) -> String
renderPosition (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- | takes a State and returns a String
--   it renders the game info such as ball pos, hole pos, hole number, stroke number and total score
renderGameInfo :: State -> String
renderGameInfo state = "Ball Pos: " ++ renderPosition (ballPosition state) ++ " | " ++
                       "Hole Pos: " ++ renderPosition (holePosition state) ++ " | " ++
                       "Hole Number: " ++ show (holeNumber state) ++ " | " ++
                       "Stroke Number: " ++ show (strokeNumber state) ++ " | " ++
                       "Total Score: " ++ show (totalScore state) ++ "\n"

-- | takes a State and returns a String
--   it renders the debug info such as Game State, Player State, Ball Direction and Tile direction
renderDebugInfo :: State -> String
renderDebugInfo state = "Game State: " ++ show (gameState state) ++ " | " ++
                        "Player State: " ++ show (playerState state) ++ " | " ++
                        "Ball Direction: " ++ show (ballDirection state) ++ " | " ++
                        "Tile Direction: " ++ show (tileDirectionByPosition (ballPosition state) (holeTiles state)) ++ " \n "

-- | takes an Env and a State and returns a String
--   renders the Stroke Power obtained from state and the Stroke Max Power obtained from env
renderPowerBar :: Env -> State -> String
renderPowerBar env state = "Stroke Power: " ++ show (strokePower state) ++ " | " ++
                           "Stroke Max: " ++ show (maxPower env) ++ "\n"

-- | takes an Env and a State and returns a String
--   calls the other render auxiliary functions above and concatenates its results 
--   to render the golf course, the power bar, the game and debug info 
renderGame :: Env -> State -> String
renderGame env state = renderTiles (holeTiles state) (ballPosition state) ++ "\n" ++
                       renderPowerBar env state ++
                       renderGameInfo state ++
                       renderDebugInfo state

-- | it handles the rendering depending on the gameState and playerState
renderString :: Game Env State String
renderString = do
    env <- ask                                     --  gets env using ask
    state <- lift get                              --  gets state using lift first since state is one level down and then get
    case gameState state of                        --  depending on the gameState case it renders different things
        Menu -> return "Welcome to Two Putt!"      --  case gameState = Menu it renders the welcome phrase
        Game -> case playerState state of          --  case gameState = Game depending on the playerState renders different things
            HoleOut -> return ("Congratulations! You've made it to the hole! Your score is: " ++ show (totalScore state) ++ "\n")  --  case playerState = HoleOut it renders the congrats and final score
            _ -> return (renderGame env state)     --  any other playerState case it calls the renderGame function
        LoadHole -> return "Loading hole..."       --  case gameState = LoadHole it renders the loading hole phrase

-- | takes Game Env State () and renders the game
renderIO :: Game Env State ()
renderIO = do
    output <- renderString            --  gets output by calling renderString
    lift $ lift (putStrLn output)     --  it uses lift twice since output is on the IO monad which is 2 levels down to print the output
    