{-|
Module      : Whole
Description : Whole program tests.
-}
module Whole (wholeTests) where

import Test.Tasty

import Utils

wholeTests :: TestTree
wholeTests = testGroup "Whole Program Tests" $
    mkWholeProgramTests "tests-public-whole-program" $
      [ newTestGroup "Interaction Tests" "interact" [1]
      , newTestGroup "Legal Moves" "moves" [1]
      , newTestGroup "Captures" "capture" [1,2]
      , newTestGroup "Loads and Saves" "loadsave" [1]
      , newTestGroup "Special Moves" "special" [1]
      , newTestGroup "Complete Games" "complete" [1]
      ]


--  -------------------------------------------------------------------- [ EOF ]
