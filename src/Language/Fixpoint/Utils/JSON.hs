{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON utilities.
module Language.Fixpoint.Utils.JSON (
  -- * To JSON
    encode

  -- ** showJSON helpers
  , (.=)
  , encodeMaybeToNull
  , encodeMaybeToNullWith

  -- * From JSON
  , decode
  , decodeMaybe

  -- ** readJSON helpers
  , readJSONObj
  , (.:)
  , jsValFromObj

  -- * Misc
  , aesonJsonBool
  , aesonJsonEq
  ) where

import           Control.Exception (displayException)
import           Control.Monad (join)
import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KMap
import           Data.ByteString (ByteString)
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import           Text.JSON ( JSON (showJSON)
                           , JSObject
                           , JSValue (JSNull, JSObject)
                           , Result (Error, Ok)
                           )
import qualified Text.JSON as JSON

-- | Helper for reading a JSON object. Errors if we receive a non-js obj.
readJSONObj :: (JSObject JSValue -> Result a) -> JSValue -> Result a
readJSONObj onObj (JSObject obj) = onObj obj
readJSONObj _ other = Error $ "Expected json obj, received: " ++ show other

-- | Helper for encoding json. Typical usage:
--
-- @
--   data Point = MkPoint { x :: Int, y :: Int, name :: String }
--
--   showJSON (MkPoint x y name) =
--     JSON.makeObj
--       [ "x" .= x,
--         "y" .= y,
--         "name" .= name
--       ]
-- @
(.=) :: JSON v => k -> v -> (k, JSValue)
key .= val = (key, showJSON val)

-- | Helper for decoding json. Typical usage:
--
-- @
--   data Point = MkPoint { x :: Int, y :: Int, name :: String }
--
--   readJSON = readJSONObj $ \v -> do
--    MkPoint
--      <$> v .: "x"
--      <*> v .: "y"
--      <*> v .: "name"
-- @
(.:) :: JSON a => JSObject JSValue -> String -> Result a
(.:) = flip JSON.valFromObj

-- | Encodes a value into a strict bytestring.
encode :: JSON a => a -> ByteString
encode = TEnc.encodeUtf8 . T.pack . JSON.encode

-- | Decodes a value from a strict bytestring.
decode :: JSON a => ByteString -> Result a
decode bs = case TEnc.decodeUtf8' bs of
  Left ex -> Error $ displayException ex
  Right t -> JSON.decode $ T.unpack t

-- | 'encodeMaybeToNullWith' with 'showJSON'.
encodeMaybeToNull :: JSON a => Maybe a -> JSValue
encodeMaybeToNull = encodeMaybeToNullWith showJSON

-- | Helper that encodes a 'Maybe' to either @a@ or @null@. This matches
-- Aeson's default behavior though it differs from JSON's, as the latter
-- encodes @Maybe a@ as @{ "Just": encode a }@ or @{ "Nothing": null }@.
encodeMaybeToNullWith :: JSON a => (a -> JSValue) -> Maybe a -> JSValue
encodeMaybeToNullWith = maybe JSNull

-- | 'decode' to 'Maybe'.
decodeMaybe :: JSON a => ByteString -> Maybe a
decodeMaybe = resultToMaybe . decode

-- | Attempts to read the string key from the json object. Returns either
-- Ok JSValue or Error, depending on success. This is an intermediate
-- step wrt 'JSON.valFromObj' and '(.:)', which check the key /and/
-- perform decoding. This is useful when we want to grab the value associated
-- to a key but not immediately decode into some type e.g. because we want to
-- apply custom decode logic.
jsValFromObj :: String -> JSON.JSObject JSON.JSValue -> JSON.Result JSValue
jsValFromObj k o =
  maybe
    (JSON.Error $ "jsValFromObj: Could not find key: " ++ show k)
    pure
    (lookup k (JSON.fromJSObject o))

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok x) = Just x
resultToMaybe (Error _) = Nothing

-- | 'aesonJsonEq' specialized to 'Bool'. Returns 'True' if the Aeson and
-- JSON encodings are equal.
aesonJsonBool :: (JSON a, ToJSON a) => a -> Bool
aesonJsonBool = and . aesonJsonEq (==) True (const False)

-- | Compares Aeson and JSON encodings. Polymorphic on the return type to allow
-- for better errors when testing i.e. instead of reducing all failures to
-- 'False', we can specialize the errors. For instance:
--
-- @
--   aesonJsonBool :: (JSON a, ToJSON a) => a -> Bool
--   aesonJsonBool = and . aesonJsonEq (==) True (const False)
--
--   aesonJsonQuickCheck :: (JSON a, ToJSON a) => a -> Property
--   aesonJsonQuickCheck = conjoin . aesonJsonEq (===) qcPass qcFail
--     where
--       qcPass = counterexample "" True
--       qcFail m = counterexample m False
-- @
aesonJsonEq ::
  forall a r.
  ( JSON a,
    ToJSON a
  ) =>
  -- | Comparison function e.g. (==)
  (forall x. (Eq x, Show x) => x -> x -> r) ->
  -- | Value representing passing.
  r ->
  -- | Value for failure, taking in a string message.
  (String -> r) ->
  -- | The value whose encoding to check.
  a ->
  -- | Resulting comparisons.
  [r]
aesonJsonEq eqFn pass onFail x = valuesEq (Aeson.toJSON x) (showJSON x)
  where
    valuesEq :: Aeson.Value -> JSON.JSValue -> [r]
    valuesEq (Aeson.Object asnObj) (JSON.JSObject jsnObj) =

      let jsnKeyList :: [(String, JSON.JSValue)]
          jsnKeyList = JSON.fromJSObject jsnObj

          recurseObj :: String -> [r]
          recurseObj strKey =
            case (KMap.lookup (fromString strKey) asnObj, L.lookup strKey jsnKeyList) of
              (Nothing, Nothing) ->
                [onFail $ "Key not found in aeson and json objects: " ++ strKey]
              (Nothing, _) ->
                [onFail $ "Key not found in aeson object: " ++ strKey]
              (_, Nothing) ->
                [onFail $ "Key not found in json object: " ++ strKey]
              (Just asnv, Just jsonv) -> valuesEq asnv jsonv

        in (KMap.size asnObj `eqFn` length jsnKeyList)
             : (jsnKeyList >>= (\(k, _) -> recurseObj k))

    valuesEq (Aeson.Array asnArr) (JSON.JSArray jsnArr) =
      (length asnArr `eqFn` length jsnArr)
        : (join $ L.zipWith valuesEq (F.toList asnArr) jsnArr)

    valuesEq (Aeson.String txt) (JSON.JSString jsstr) =
      [txt `eqFn` T.pack (JSON.fromJSString jsstr)]
    valuesEq (Aeson.Number sci) (JSON.JSRational _ rational) =
      [toRational sci `eqFn` rational]
    valuesEq (Aeson.Bool b1) (JSON.JSBool b2) = [b1 `eqFn` b2]
    valuesEq Aeson.Null JSON.JSNull = [pass]
    valuesEq asn jsn = [onFail msg]
      where
        msg =
          mconcat
            [ "Received different types.\nAeson: ",
              show asn,
              "\nJSON:",
              show jsn
            ]