{-# LANGUAGE OverloadedStrings #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (encodePretty) where

import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (intersperse)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)


type Indent = Int

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing 
--  JSON-ByteStrings for human readers.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodeUtf8 . toLazyText . fromValue 0 . toJSON

fromValue :: Indent -> Value -> Builder
fromValue ind = go
  where
    go (Array v)  = fromCompound ind ("[","]") fromValue (V.toList v)
    go (Object m) = fromCompound ind ("{","}") fromPair (H.toList m)
    go v          = Aeson.fromValue v

fromCompound :: Indent
             -> (Builder, Builder)
             -> (Indent -> a -> Builder)
             -> [a]
             -> Builder
fromCompound ind (delimL,delimR) fromItem items =
    delimL `mappend` items' `mappend` delimR
  where
    items'  = if null items then mempty
                else mconcat
                    [ "\n"
                    , mconcat . intersperse ",\n" $
                        map (\i -> fromIndent (ind+1) `mappend`
                                   fromItem (ind+1) i)
                            items
                    , "\n"
                    , fromIndent ind
                    ]

fromPair :: Indent -> (Text, Value) -> Builder
fromPair ind (k,v) =
    Aeson.fromValue (toJSON k) `mappend` ": " `mappend` fromValue ind v

fromIndent :: Indent -> Builder
fromIndent ind = mconcat $ replicate (ind*4) " "
