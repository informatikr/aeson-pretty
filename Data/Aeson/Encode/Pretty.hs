{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (
    -- * Simple Pretty-Printing
    encodePretty, encodePrettyToTextBuilder,
    
    -- * Pretty-Printing with Configuration Options
    encodePretty', encodePrettyToTextBuilder',
    Config (..), defConfig,
    -- ** Sorting Keys in Objects
    -- |With the Aeson library, the order of keys in objects is undefined due
    --  objects being implemented as HashMaps. To allow user-specified key
    --  orders in the pretty-printed JSON, 'encodePretty'' can be configured
    --  with a comparison function. These comparison functions can be composed
    --  using the 'Monoid' interface. Some other useful helper functions to keep
    --  in mind are 'comparing' and 'on'.
    --  
    --  Consider the following deliberately convoluted example, demonstrating
    --  the use of comparison functions:
    --
    --  An  object might pretty-print as follows
    --
    --  > {
    --  >   "baz": ...,
    --  >   "bar": ...,
    --  >   "foo": ...,
    --  >   "quux": ...,
    --  > }
    --
    --  which is clearly a confusing order of keys. By using a comparison
    --  function such as
    --
    --  > comp :: Text -> Text -> Ordering
    --  > comp = keyOrder ["foo","bar"] `mappend` comparing length
    --
    --  we can achieve the desired neat result:
    --
    --  > {
    --  >   "foo": ...,
    --  >   "bar": ...,
    --  >   "baz": ...,
    --  >   "quux": ...,
    --  > }
    --
    
    mempty,
    -- |Serves as an order-preserving (non-)sort function. Re-exported from
    --  "Data.Monoid".
    compare,
    -- |Sort keys in their natural order, i.e. by comparing character codes.
    -- Re-exported from the Prelude and "Data.Ord"
    keyOrder
) where

import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Ord
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)

data PState = PState { pstIndent :: Int
                     , pstLevel  :: Int
                     , pstSort   :: [(Text, Value)] -> [(Text, Value)]
                     }

data Config = Config
    { confIndent  :: Int
      -- ^ Indentation spaces per level of nesting
    , confCompare :: Text -> Text -> Ordering
      -- ^ Function used to sort keys in objects
    }

-- |Sort keys by their order of appearance in the argument list.
--
--  Keys that are not present in the argument list are considered to be greater
--  than any key in the list and equal to all keys not in the list. I.e. keys
--  not in the argument list are moved to the end, while their order is
--  preserved.
keyOrder :: [Text] -> Text -> Text -> Ordering
keyOrder ks = comparing $ \k -> fromMaybe maxBound (elemIndex k ks)


-- |The default configuration: indent by four spaces per level of nesting, do
--  not sort objects by key.
--
--  > defConfig = Config { confIndent = 4, confCompare = mempty }
defConfig :: Config
defConfig = Config { confIndent = 4, confCompare = mempty }

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing 
--  JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePretty :: ToJSON a => a -> ByteString
encodePretty = encodePretty' defConfig

-- |A variant of 'encodePretty' that takes an additional configuration
--  parameter.
encodePretty' :: ToJSON a => Config -> a -> ByteString
encodePretty' conf = encodeUtf8 . toLazyText . encodePrettyToTextBuilder' conf

-- |A drop-in replacement for aeson's 'Aeson.encodeToTextBuilder' function,
--  producing JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePrettyToTextBuilder :: ToJSON a => a -> Builder
encodePrettyToTextBuilder = encodePrettyToTextBuilder' defConfig

-- |A variant of 'encodeToTextBuilder' that takes an additional configuration
--  parameter.
encodePrettyToTextBuilder' :: ToJSON a => Config -> a -> Builder
encodePrettyToTextBuilder' Config{..} = fromValue st . toJSON
  where
    st       = PState confIndent 0 condSort
    condSort = sortBy (confCompare `on` fst)


fromValue :: PState -> Value -> Builder
fromValue st@PState{..} = go
  where
    go (Array v)  = fromCompound st ("[","]") fromValue (V.toList v)
    go (Object m) = fromCompound st ("{","}") fromPair (pstSort (H.toList m))
    go v          = Aeson.encodeToTextBuilder v

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st@PState{..} (delimL,delimR) fromItem items = mconcat
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
fromPair st (k,v) = Aeson.encodeToTextBuilder (toJSON k) <> ": " <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState{..} = mconcat $ replicate (pstIndent * pstLevel) " "
