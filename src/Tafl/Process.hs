{- |

The `Process` module implements the game commands.

-}
module Tafl.Process
  ( processCommand
  , processCommandStr
  , printError
  ) where

import System.Exit

import Tafl.Core
import Tafl.Logic

-- | Process user commands and updates the GameState.
-- Returns a `TaflError`
processCommand :: GameState
               -> Command
               -> IO (Either TaflError GameState)

processCommand st Help = do
  putStrLn help_text
  pure $ Right st
processCommand st Exit = do
  putStrLn "Good Bye!"
  exitWith ExitSuccess -- Skeleton Code

processCommand st Start =
  if inGame st
    then pure $ Left (InvalidCommand "Can't Start")
    else do
      let newSt = defaultGameState{inGame=True}
      putStrLn "Starting Game."
      pure $ Right newSt

processCommand st Stop =
  if not (inGame st)
    then pure $ Left (InvalidCommand "Can't Stop")
    else do
      let newSt = defaultGameState
      putStrLn "Stopping Game."
      pure $ Right newSt

-- The remaining commands are to be added here.

processCommand st (Move src des)
  | inGame st = pure $ Left NotReadyCommand
  | length src /=2 ||length des /=2 = pure $ Left MalformedCommand
  | otherwise =
        do
          --THIS IS FOR DEBUGGING -- KNOWN ERROR 

          -- putStr("Source: " ++ src )
          -- putStrLn(show $ locToPiece st (stringToLoc src))
          -- putStr("Des: " ++ des )
          -- putStrLn(show $ locToPiece st (stringToLoc des))
          -- putStr("Validity: ")
          -- putStrLn (show (isValidPath st (stringToLoc src) (stringToLoc des)))
          -- putStr("getPiecesOnPath: ")
          -- let path = map (show) $ getPiecesOnPath st (pathGen (stringToLoc src) (stringToLoc des))
          -- putStrLn (unlines(path))

          --END OF DEBUGGING CODE

          -- Check if the source type matches the turn
          -- (aka Objects cannot move Lambdas pieces and otherwise)
          -- Check if the path is valid
          -- (aka there are only E pieces on the path)
          let srcLoc = stringToLoc src
          let desLoc = stringToLoc des
          let srcType = locToPiece st (stringToLoc src)

          if isValidPath st srcLoc desLoc && (srcType == O && gameTurn st == Objects)||(srcType == L && gameTurn st == Lambdas)||(srcType == G && gameTurn st == Lambdas)
            then do
              putStrLn "Move Successful"
              --DoMoving
              let newMove = doMoving st srcLoc desLoc
              --DoCapture  --> NotYetImplemented
              --switchSide
              let newSide = switchSide newMove
              pure $ Right newSide
          --Invalid Turn or Path
          else pure $ Left InvalidMove

processCommand st (Save fname) =
  if inGame st
    then
      do
        putStrLn ("State saved in " ++ fname)
        pure $ Right st
    else pure $ Left NotReadyCommand

processCommand st (Load fname) =
  if not(inGame st)
    then
      do
        let newSt = st {inGame=True}
        putStrLn ("State loaded from " ++ fname)
        pure $ Right newSt
    else pure $ Left NotReadyCommand

processCommand st Show =
  if inGame st
    then
      do
        putStrLn (stateToString st)
        pure $ Right st
    else
      do
        putStrLn (stateToString st)
        pure $ Left NotReadyCommand

processCommand st _ = pure $ Left UnknownCommand



-- | Process a user given command presented as a String, and update
-- the GameState.
processCommandStr :: GameState
                  -> String
                  -> IO (Either TaflError GameState)
processCommandStr st str =
  case commandFromString str of
    Left err   -> pure (Left err)
    Right cmd' -> processCommand st cmd'


-- | Print an Error to STDOUT.
printError :: TaflError -> IO ()
printError InvalidMove =
  putStrLn "Invalid Move!"
printError NotYetImplemented =
  putStrLn "Not Yet Implemented."
printError UnknownCommand =
  putStrLn "The command was not recognised"
printError MalformedCommand =
  putStrLn "The entered command was malformed."
printError NotReadyCommand =
  putStrLn "The command cannot be used."
printError (InvalidCommand msg) = do
  putStrLn "You entered an invalid command:"
  putStr "\t"
  putStrLn msg
