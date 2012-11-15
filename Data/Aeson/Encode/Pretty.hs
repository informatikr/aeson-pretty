{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty
( Config (..)
, defConfig
, encodePretty
, encodePretty'
) where

import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (intersperse, sortBy)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Function (on)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)


data PState = PState { pstIndent :: Int
                     , pstLevel  :: Int
                     , pstSort   :: [(Text, Value)] -> [(Text, Value)]
                     }

data Config = Config { confIndent  :: Int  -- ^ Spaces per level
                     , confCompare :: Maybe (Text -> Text -> Ordering)
                                           -- ^ Sort objects by key
                     }

-- |The default configuration: indent by four spaces per level of nesting, do
--  not sort objects by key.
--
--  > defConfig = Config { confIndent = 4, confSort = Nothing }
defConfig :: Config
defConfig = Config { confIndent = 4, confCompare = Nothing }

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing 
--  JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' defConfig

-- |A variant of 'encodePretty' that takes an additional configuration
--  parameter.
encodePretty' :: ToJSON a => Config -> a -> ByteString
encodePretty' (Config {..}) = encodeUtf8 . toLazyText . fromValue st . toJSON
  where
    st = PState confIndent 0 condSort

    condSort :: [(Text, Value)] -> [(Text, Value)]
    condSort = maybe id (\c -> sortBy (c `on` fst)) confCompare

fromValue :: PState -> Value -> Builder
fromValue st@(PState {..}) = go
  where
    go (Array v)  = fromCompound st ("[","]") fromValue (V.toList v)
    go (Object m) = fromCompound st ("{","}") fromPair (pstSort (H.toList m))
    go v          = Aeson.fromValue v

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st@(PState {..}) (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else "\n" <> items' <> "\n" <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse ",\n" $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pstLevel = pstLevel + 1 }

fromPair :: PState -> (Text, Value) -> Builder
fromPair st (k,v) = Aeson.fromValue (toJSON k) <> ": " <> fromValue st v

fromIndent :: PState -> Builder
fromIndent (PState {..}) = mconcat $ replicate (pstIndent * pstLevel) " "

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
infixr 6 <>
