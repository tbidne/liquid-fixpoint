{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONTests (tests) where

import Arbitrary ()
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Language.Fixpoint.Horn.Types (Tag)
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
import qualified Language.Fixpoint.Types.Constraints as Constraints
import Language.Fixpoint.Types.Errors (FixResult (Crash, Safe, Unsafe))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.QuickCheck (property, testProperty, total)

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
      testUnsafeEncode,
      testCrashEncode
    ]

testSafeEncode :: TestTree
testSafeEncode = goldenVsFile "Tests Safe encode" gpath apath $ do
  let encoded = Aeson.encode fixResult
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "safe"
    fixResult :: FixResult Integer
    fixResult = Safe stats

testUnsafeEncode :: TestTree
testUnsafeEncode = goldenVsFile "Tests Unsafe encode" gpath apath $ do
  let encoded = Aeson.encode fixResult
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "unsafe"
    fixResult :: FixResult Integer
    fixResult = Unsafe stats [1, 2, 3]

testCrashEncode :: TestTree
testCrashEncode = goldenVsFile "Tests Crash encode" gpath apath $ do
  let encoded = Aeson.encode fixResult
  BS.writeFile apath (BSL.toStrict encoded)
  where
    (gpath, apath) = getGoldenPaths "crash"
    fixResult :: FixResult Integer
    fixResult = Crash [(1, Just "\n"), (5, Nothing), (9, Just "ccc")] "str"

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
    [ testEncodeFixResultArbitrary,
      testEncodeHornResultArbitrary,
      testEncodeFqResultArbitrary
    ]

testEncodeFixResultArbitrary :: TestTree
testEncodeFixResultArbitrary = testProperty "encode arbitrary FixResult succeeds" $
  property $ \(fixResult :: FixResult Integer) ->
    total $ Aeson.encode fixResult

testEncodeHornResultArbitrary :: TestTree
testEncodeHornResultArbitrary = testProperty "encode arbitrary horn Result succeeds" $
  property $ \(result :: Constraints.Result (Integer, Tag)) ->
    total $ Aeson.encode result

testEncodeFqResultArbitrary :: TestTree
testEncodeFqResultArbitrary = testProperty "encode arbitrary fq Result succeeds" $
  property $ \(result :: Constraints.Result (Integer, ())) ->
    total $ Aeson.encode result

getGoldenPaths :: FilePath -> (FilePath, FilePath)
getGoldenPaths fileName = (golden, actual)
  where
    prefix = "tests" </> "tasty" </> "json" </> fileName
    golden = prefix ++ "-golden.json"
    actual = prefix ++ "-actual.json"
