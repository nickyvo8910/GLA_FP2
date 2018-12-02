{- |

This module defines the games two modes of interactions: REPL and REPL.

-}
module Tafl.Interact
  ( repl
  ) where

import System.Exit
import System.IO

import Tafl.Core
import Tafl.Process


-- | Core REPL for processing user actions.
repl :: GameState -- ^ The initial starting start of the game
     -> IO ()
repl st = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  LineBuffering
    doREPL st

  where
    doREPL :: GameState -> IO ()
    doREPL st = do
      if inGame st
        then if gameTurn st == Objects then putStr "tafl game O>" else
          putStr "tafl game L>"
      else putStr "tafl> "
      raw_cmd <- getLine
      result  <- processCommandStr st raw_cmd
      case result of
        (Left err) -> do
          printError err
          repl st

        (Right newSt) -> repl newSt
