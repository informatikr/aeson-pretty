{-# LANGUAGE OverloadedStrings #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (encodePretty, encodePretty') where

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


type Indent = (Int, Int) -- (spaces per lvl, lvl)

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing 
--  JSON-ByteStrings for human readers.
--
--  Indents by four spaces per nesting-level.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' 4 -- default indentation is four spaces

-- |A variant of 'encodePretty' that takes an additional parameter: the number
--  of spaces to indent per nesting-level.
encodePretty' :: ToJSON a => Int -> a -> ByteString
encodePretty' spacesPerLvl = encodeUtf8 . toLazyText . fromValue ind . toJSON
  where
    ind = (spacesPerLvl, 0)

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
                map (\item -> fromIndent ind' <> fromItem ind' item)
                    items
    ind' = let (spacesPerLvl, lvl) = ind in (spacesPerLvl, lvl + 1)

fromPair :: Indent -> (Text, Value) -> Builder
fromPair ind (k,v) = Aeson.fromValue (toJSON k) <> ": " <> fromValue ind v

fromIndent :: Indent -> Builder
fromIndent (spacesPerLvl, lvl) = mconcat $ replicate (spacesPerLvl * lvl) " "

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
infixr 6 <>
