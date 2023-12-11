{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified InterpretTests
import qualified JSONTests
import qualified ParserTests
import qualified ShareMapTests
import qualified SimplifyTests
import Test.Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import qualified UndoANFTests

main :: IO ()
main =
  defaultMain $
    localOption OnPass $
      testGroup
        "Tests"
        [ JSONTests.tests,
          ParserTests.tests,
          ShareMapTests.tests,
          SimplifyTests.tests,
          InterpretTests.tests,
          UndoANFTests.tests
        ]
