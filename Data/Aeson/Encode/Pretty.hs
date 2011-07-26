{-# LANGUAGE OverloadedStrings #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (encodePretty) where

import Blaze.ByteString.Builder (Builder, toLazyByteString, fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.Map (assocs)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text (Text)
import Data.Vector (toList)


type Indent = Int

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing 
--  JSON-ByteStrings for human readers.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = toLazyByteString . fromValue 0 . toJSON

fromValue :: Indent -> Value -> Builder
fromValue ind = go
  where
    go (Array v)  = fromCompound ind ('[',']') fromValue (toList v)
    go (Object v) = fromCompound ind ('{','}') fromPair (assocs v)
    go v          = Aeson.fromValue v

fromCompound :: Indent
               -> (Char, Char)
               -> (Indent -> a -> Builder)
               -> [a]
               -> Builder
fromCompound ind (delimL,delimR) fromItem items =    
    fromChar delimL `mappend` items' `mappend` fromChar delimR
  where
    newLine = fromChar '\n'
    items'  = if null items then mempty
                else mconcat
                    [ newLine
                    , mconcat . intersperse (fromChar ',' `mappend` newLine) $
                        map (\i -> fromIndent (ind+1) `mappend`
                                   fromItem (ind+1) i)
                            items
                    , newLine
                    , fromIndent ind
                    ]

fromPair :: Indent -> (Text, Value) -> Builder
fromPair ind (k,v) =
    mconcat [ Aeson.fromValue (toJSON k)
            , fromByteString ": "
            , fromValue ind v
            ]

fromIndent :: Indent -> Builder
fromIndent ind = mconcat $ replicate (ind*4) $ fromChar ' '
