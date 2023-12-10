{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified JSONTests
import qualified ParserTests
import qualified ShareMapTests
import qualified SimplifyTests
import qualified InterpretTests
import qualified UndoANFTests
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ JSONTests.tests
  , ParserTests.tests
  , ShareMapTests.tests
  , SimplifyTests.tests
  , InterpretTests.tests
  , UndoANFTests.tests
  ]
