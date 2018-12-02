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
  exitWith ExitSuccess

processCommand st Start = do
  if(inGame st == True)
    then do pure $ Left (InvalidCommand "Can't Start")
    else
     do
       let newSt = st {inGame=True}
       putStrLn "Starting Game."
       -- putStrLn (stateToString newSt)
       pure $ Right newSt

processCommand st Stop = do
  if(inGame st == False)
    then do pure $ Left (InvalidCommand "Can't Stop")
    else
      do
        let newSt = defaultGameState
        putStrLn "Stopping Game."
        pure $ Right newSt

-- The remaining commands are to be added here.

processCommand st (Move src des) = do
  if(inGame st == True)
    then
      do
        if (length(src)/=2 ||length(des)/=2 ) then do pure $ Left (MalformedCommand)
          else
            do
              putStr("Source: " ++ src )
              putStrLn(show $ locToPiece st (stringToLoc src))
              putStr("Des: " ++ des )
              putStrLn(show $ locToPiece st (stringToLoc des))

              putStr("Validity: ")
              putStrLn (show (isValidPath st (stringToLoc src) (stringToLoc des)))

<<<<<<< HEAD
              -- Check if the srcTYpe matches the turn
              let srcLoc = stringToLoc src
              let desLoc = stringToLoc des

              let srcType = locToPiece st (stringToLoc src)


              if (srcType == O && gameTurn st == Objects)||(srcType == L && gameTurn st == Lambdas)||(srcType == G && gameTurn st == Lambdas)
                then do
                  -- Check if the path is valid
                  if isValidPath st srcLoc desLoc
                    then do
                      putStrLn ("Move Successful")
                      --DoMoving
                      let newMove = doMoving st srcLoc desLoc
                      --DoCapture

                      --switchSide
                      let newSide = switchSide $ newMove
                      pure $ Right newSide
                  else do pure $ Left (InvalidMove)
              else do pure $ Left (InvalidMove)
    else do pure $ Left (NotReadyCommand)
=======
              if (isValidPath st (stringToLoc src) (stringToLoc des))
                then
                  do
                    let afterMoveSt = doMoving st (stringToLoc src) (stringToLoc des)
                    let newSt = switchSide afterMoveSt
                    putStrLn ("Move Successful")
                    putStrLn (stateToString afterMoveSt)
                    pure $ Right afterMoveSt
                else
                  do
                    pure $ Left (InvalidMove)
    else
     do pure $ Left (NotReadyCommand)
>>>>>>> eb6cdcf1c5e1352c15361c9e963e5fc69ffb9713

processCommand st (Save fname) = do
  if(inGame st == True)
    then
      do
        putStrLn ("State saved in " ++ fname)
        pure $ Right st
    else
      do pure $ Left (NotReadyCommand)

processCommand st (Load fname) = do
  if(inGame st == False)
    then
      do
        let newSt = st {inGame=True}
        putStrLn ("State loaded from " ++ fname)
        pure $ Right newSt
    else
      do pure $ Left (NotReadyCommand)

processCommand st (Show) = do
  if(inGame st == True)
    then
      do
        putStrLn (stateToString st)
        pure $ Right st
    else
      do pure $ Left (NotReadyCommand)

processCommand st _ = pure $ Left (UnknownCommand)



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
printError (InvalidMove) = do
  putStrLn "Invalid Move!"
printError (NotYetImplemented) = do
  putStrLn "Not Yet Implemented."
printError (UnknownCommand) = do
  putStrLn "The command was not recognised"
printError (MalformedCommand) = do
  putStrLn "The entered command was malformed."
printError (NotReadyCommand) = do
  putStrLn "The command cannot be used."
printError (InvalidCommand msg) = do
  putStrLn "You entered an invalid command:"
  putStr "\t"
  putStrLn msg
