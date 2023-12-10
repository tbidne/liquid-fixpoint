{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONTests (tests) where

import           Arbitrary ()
import           Data.String (IsString)
import qualified Data.ByteString as BS
import           Language.Fixpoint.Solver.Stats (Stats(..))
import           Language.Fixpoint.Types.Errors (FixResult(..))
import qualified Language.Fixpoint.Utils.JSON as LiquidJSON
import           System.FilePath ((</>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))
import           Test.Tasty.QuickCheck ( Testable(property)
                                       , testProperty
                                       , (===)
                                       )
import Text.JSON (JSON, Result (Ok))

tests :: TestTree
tests =
  testGroup "JSONTests"
    [ specs,
      properties
    ]

specs :: TestTree
specs =
  testGroup "Specs"
    [ testSafeEncode,
      testUnsafeEncode,
      testCrashEncode
    ]

testSafeEncode :: TestTree
testSafeEncode = testCase "Tests Safe encode" $ do
  expected @=? LiquidJSON.encode fixResult
  testDecode "safe.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Safe stats
    expected :: (IsString a, Monoid a) => a
    expected =
      mconcat
        [ "{",
            "\"tag\":\"Safe\",",
            "\"contents\":{",
              "\"numCstr\":1,",
              "\"numIter\":2,",
              "\"numBrkt\":3,",
              "\"numChck\":4,",
              "\"numVald\":5",
            "}",
          "}"
        ]

testUnsafeEncode :: TestTree
testUnsafeEncode = testCase "Tests Unsafe encode" $ do
  expected @=? LiquidJSON.encode fixResult
  testDecode "unsafe.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Unsafe stats [1,2,3]
    expected :: (IsString a, Monoid a) => a
    expected =
      mconcat
        [ "{",
            "\"tag\":\"Unsafe\",",
            "\"contents\":[",
              "{\"numCstr\":1,",
              "\"numIter\":2,",
              "\"numBrkt\":3,",
              "\"numChck\":4,",
              "\"numVald\":5},",
              "[1,2,3]",
            "]",
          "}"
        ]

testCrashEncode :: TestTree
testCrashEncode = testCase "Tests Crash encode" $ do
  expected @=? LiquidJSON.encode fixResult
  testDecode "crash.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Crash [(1, Just "\n"), (5, Nothing), (9, Just "ccc")] "str"
    expected :: (IsString a, Monoid a) => a
    expected =
      mconcat
        [ "{",
            "\"tag\":\"Crash\",",
            "\"contents\":[",
              "[[1,\"\\n\"],",
              "[5,null],",
              "[9,\"ccc\"]],",
              "\"str\"",
            "]",
          "}"
        ]

testDecode :: (Eq a, JSON a, Show a) => FilePath -> a -> IO ()
testDecode fileName expected = do
  contents <- BS.readFile path
  Ok expected @=? LiquidJSON.decode contents
  where
    path = "tests" </> "tasty" </> "json" </> fileName

stats :: Stats
stats = Stats
  { numCstr = 1,
    numIter = 2,
    numBrkt = 3,
    numChck = 4,
    numVald = 5
  }

properties :: TestTree
properties =
  testGroup "Properties"
    [ testRoundtrip
    ]

testRoundtrip :: TestTree
testRoundtrip = testProperty "decode . encode round trips" $
  property $ \(fixResult :: FixResult Integer) ->
    Ok fixResult === LiquidJSON.decode (LiquidJSON.encode fixResult)