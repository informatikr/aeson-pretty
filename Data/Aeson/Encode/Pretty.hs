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
import Data.Text.Lazy.Builder (Builder, toLazyText)
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
fromCompound ind (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else "\n" <> items' <> "\n" <> fromIndent ind
    , delimR
    ]
  where
    items' = mconcat . intersperse ",\n" $
                map (\item -> fromIndent (ind+1) <> fromItem (ind+1) item)
                    items

fromPair :: Indent -> (Text, Value) -> Builder
fromPair ind (k,v) = Aeson.fromValue (toJSON k) <> ": " <> fromValue ind v

fromIndent :: Indent -> Builder
fromIndent ind = mconcat $ replicate (ind*4) " "

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
infixr 6 <>
