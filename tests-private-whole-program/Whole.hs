{-|
Module      : Whole
Description : Whole program tests.
-}
module Whole (wholeTests) where

import Test.Tasty

import Utils

wholeTests :: TestTree
wholeTests = testGroup "Whole Program Tests" $
    mkWholeProgramTests "tests-private-whole-program"
      [ newTestGroup "Captures" "capture" [1] ]


--  -------------------------------------------------------------------- [ EOF ]
