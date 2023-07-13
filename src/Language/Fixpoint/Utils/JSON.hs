{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | JSON utilities.
module Language.Fixpoint.Utils.JSON (
  -- * To JSON
    encode
  , (.=)

  -- * From JSON
  , decode
  , decodeMaybe
  , readJSONObj
  , (.:)
  ) where

import           Control.Exception (displayException)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import           Text.JSON ( JSON (showJSON)
                           , JSObject
                           , JSValue (JSObject)
                           , Result (Error, Ok)
                           )
import qualified Text.JSON as JSON

-- | Helper for reading a JSON object. Errors if we receive a non-js obj.
readJSONObj :: (JSObject JSValue -> Result a) -> JSValue -> Result a
readJSONObj onObj (JSObject obj) = onObj obj
readJSONObj _ other = Error $ "Expected json obj, received: " ++ show other

-- | Helper for encoding json.
(.=) :: JSON v => k -> v -> (k, JSValue)
key .= val = (key, showJSON val)

-- | Helper for decoding json.
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

-- | 'decode' to 'Maybe'.
decodeMaybe :: JSON a => ByteString -> Maybe a
decodeMaybe = resultToMaybe . decode

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok x) = Just x
resultToMaybe (Error _) = Nothing