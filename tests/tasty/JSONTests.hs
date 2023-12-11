{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONTests (tests) where

import Arbitrary ()
import qualified Data.ByteString as BS
import Language.Fixpoint.Solver.Stats
  ( Stats
      ( Stats,
        numBrkt,
        numChck,
        numCstr,
        numIter,
        numVald
      ),
  )
import Language.Fixpoint.Types.Constraints (resStatus)
import qualified Language.Fixpoint.Types.Constraints as Constraints
import Language.Fixpoint.Types.Errors (FixResult (Crash, Safe, Unsafe))
import qualified Language.Fixpoint.Utils.JSON as LiquidJSON
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck
  ( Testable (property),
    testProperty,
    (===)
  )
import Text.JSON (JSON, Result (Ok))

tests :: TestTree
tests =
  testGroup
    "JSON"
    [ specs,
      properties
    ]

specs :: TestTree
specs =
  testGroup
    "Specs"
    [ testSafeEncode,
      testSafeDecode,
      testUnsafeEncode,
      testUnsafeDecode,
      testCrashEncode,
      testCrashDecode
    ]

testSafeEncode :: TestTree
testSafeEncode = goldenVsFile "Safe encode" gpath apath $ do
  let encoded = LiquidJSON.encode fixResult
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "safe"
    fixResult :: FixResult Integer
    fixResult = Safe stats

testSafeDecode :: TestTree
testSafeDecode = testCase "Safe decode" $ do
  testDecode "safe-golden.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Safe stats

testUnsafeEncode :: TestTree
testUnsafeEncode = goldenVsFile "Unsafe encode" gpath apath $ do
  let encoded = LiquidJSON.encode fixResult
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "unsafe"
    fixResult :: FixResult Integer
    fixResult = Unsafe stats [1, 2, 3]

testUnsafeDecode :: TestTree
testUnsafeDecode = testCase "Unsafe decode" $ do
  testDecode "unsafe-golden.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Unsafe stats [1, 2, 3]

testCrashEncode :: TestTree
testCrashEncode = goldenVsFile "Crash encode" gpath apath $ do
  let encoded = LiquidJSON.encode fixResult
  BS.writeFile apath encoded
  where
    (gpath, apath) = getGoldenPaths "crash"
    fixResult :: FixResult Integer
    fixResult = Crash [(1, Just "\n"), (5, Nothing), (9, Just "ccc")] "str"

testCrashDecode :: TestTree
testCrashDecode = testCase "Crash decode" $ do
  testDecode "crash-golden.json" fixResult
  where
    fixResult :: FixResult Integer
    fixResult = Crash [(1, Just "\n"), (5, Nothing), (9, Just "ccc")] "str"

testDecode :: (Eq a, JSON a, Show a) => FilePath -> a -> IO ()
testDecode fileName expected = do
  contents <- BS.readFile path
  Ok expected @=? LiquidJSON.decode contents
  where
    path = jsonDir </> fileName

stats :: Stats
stats =
  Stats
    { numCstr = 1,
      numIter = 2,
      numBrkt = 3,
      numChck = 4,
      numVald = 5
    }

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testFixResultRoundtrip,
      testDecodePreservesHornStatus,
      testDecodePreservesFQStatus
    ]

testFixResultRoundtrip :: TestTree
testFixResultRoundtrip = testProperty "decode . encode FixResult round trips" $
  property $ \(fixResult :: FixResult Integer) ->
    Ok fixResult === LiquidJSON.decode (LiquidJSON.encode fixResult)

testDecodePreservesHornStatus :: TestTree
testDecodePreservesHornStatus = testProperty "decode . encode Result preserves Horn status" $
  property $ \(result :: Constraints.Result (Integer, Constraints.Tag)) ->
    let zeroedResult = mempty {resStatus = resStatus result}
     in Ok zeroedResult === LiquidJSON.decode (LiquidJSON.encode result)

testDecodePreservesFQStatus :: TestTree
testDecodePreservesFQStatus = testProperty "decode . encode Result preserves FQ status" $
  property $ \(result :: Constraints.Result (Integer, ())) ->
    let zeroedResult = mempty {resStatus = resStatus result}
     in Ok zeroedResult === LiquidJSON.decode (LiquidJSON.encode result)

getGoldenPaths :: FilePath -> (FilePath, FilePath)
getGoldenPaths fileName = (golden, actual)
  where
    prefix = jsonDir </> fileName
    golden = prefix ++ "-golden.json"
    actual = prefix ++ "-actual.json"

jsonDir :: FilePath
jsonDir = "tests" </> "tasty" </> "json"
