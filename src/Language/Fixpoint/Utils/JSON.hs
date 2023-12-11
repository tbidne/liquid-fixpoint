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
  ) where

import           Control.Exception (displayException)
import           Data.ByteString (ByteString)
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