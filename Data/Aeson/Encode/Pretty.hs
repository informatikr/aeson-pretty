{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
encodePretty :: Value -> ByteString
encodePretty = toLazyByteString . fromValue 0

fromValue :: Indent -> Value -> Builder
fromValue lvl = go
  where
    go (Array v)  = renderCompound lvl ('[',']') renderListItem (toList v)
    go (Object v) = renderCompound lvl ('{','}') renderPair (assocs v)
    go v          = Aeson.fromValue v

renderCompound :: Indent
               -> (Char, Char)
               -> (Indent -> a -> Builder)
               -> [a]
               -> Builder
renderCompound lvl (delimL,delimR) render content =    
    fromChar delimL `mappend` content' `mappend` fromChar delimR
  where
    content' = if null content then mempty
                else mconcat
                    [ newLine
                    , mconcat . intersperse (fromChar ',' `mappend` newLine) $
                        map (render $ lvl+1) content
                    , newLine
                    , indent lvl
                    ]
    newLine  = fromChar '\n'

renderListItem :: Indent -> Value -> Builder
renderListItem lvl v = indent lvl `mappend` fromValue lvl v

renderPair :: Indent -> (Text, Value) -> Builder
renderPair lvl (k,v) =
    mconcat [ indent lvl
            , Aeson.fromValue (toJSON k)
            , fromByteString ": "
            , fromValue lvl v
            ]

indent :: Indent -> Builder
indent lvl = mconcat $ replicate (lvl*4) $ fromChar ' '
