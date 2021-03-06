{- |
This module defines several core data structures used by the game.
-}
module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , Piece(..)
  , Loc
  , Board
  , Side(..)
  , defaultGameState
  , initGameState
  , commandFromString
  , help_text
  ) where
import           Data.List
import           System.Exit

-- | The core game state that captures the state of the board, and
-- whether we are playing a game or not.
--
-- You will need to extend this to present the board.


type Loc = (Int, Int)
  -- Represent a location (square) on the gameboard
data Piece = O | G | L | X | E | R deriving (Show, Read, Eq, Ord)
  -- Types of Pieces on the board with addiontional R for error catching
data Side = Lambdas | Objects deriving (Show,Eq)
  --Represent 2 sides of the game
type Board = [String]
  --Represent the gameBoard

data GameState = GameState
  { inGame     :: Bool
  , inTestMode :: Bool
  , gameTurn   :: Side
  , gameBoard  :: Board
  }

defaultGamePlacing =["   OOO   ",
                    "    O    ",
                    "    G    ",
                    "O   G   O",
                    "OOGGLGGOO",
                    "O   G   O",
                    "    G    ",
                    "    O    ",
                    "   OOO   "]


defaultGameState :: GameState
defaultGameState = GameState False False Objects defaultGamePlacing

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ GameState False b Objects [""]
initGameState (Just f) b = pure $ Left NotYetImplemented

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = InvalidCommand String
               | UnknownCommand
               | NotYetImplemented
               | MalformedCommand
               | NotReadyCommand
               | InvalidMove

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop
             | Move String String
             | Load String
             | Save String
             | Show

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"]         -> Right Help
    ["exit"]         -> Right Exit
    ["start"]        -> Right Start
    ["stop"]         -> Right Stop
    ["move",src,des] -> Right (Move src des)

    ["load",fname]   ->Right (Load fname)
    ["save",fname]   ->Right (Save fname)

    ["show"]         -> Right Show


    -- You need to specify how to recognise the remaining commands and their arguments here.

    _                -> Left UnknownCommand

commandFromString _  = Left UnknownCommand


help_text :: String
help_text = unlines $  -- Skeleton Code
     [ "Tafl Help text:", ""]
  ++ map prettyCmdHelp
       [ ("help",  "Displays this help text." )
       , ("exit",  "Exits the Command Prompt.")
       , ("start", "Initiates a game."        )
       , ("stop",  "Stops a game."            )
       , ("show",  "Show the current status of a game."            )
       , ("move src des",  "Moving a piece from src <rowcol> to des <rowcol>."            )
       , ("save fname",  "Saving a game. -- NotYetImplemented"            )
       , ("load fname",  "Loading a game. -- NotYetImplemented"            )
       ]
  where
    prettyCmdHelp :: (String, String) -> String
    prettyCmdHelp (cmd, help) = concat ["\t:", cmd, "\t", " "] ++ help
