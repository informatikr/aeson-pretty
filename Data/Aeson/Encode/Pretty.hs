{-# LANGUAGE OverloadedStrings, RecordWildCards, CPP #-}

-- |Aeson-compatible pretty-printing of JSON 'Value's.
module Data.Aeson.Encode.Pretty (
    -- * Simple Pretty-Printing
    encodePretty, encodePrettyToTextBuilder,

    -- * Pretty-Printing with Configuration Options
    encodePretty', encodePrettyToTextBuilder',
    Config (..), defConfig,
    Indent(..), NumberFormat(..),
    -- ** Sorting Keys in Objects
    -- |With the Aeson library, the order of keys in objects is undefined due to
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

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
#endif
import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
#if !MIN_VERSION_aeson(2,0,0)
import qualified Data.HashMap.Strict as H (toList)
#endif
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif
import qualified Data.Scientific as S (Scientific, FPFormat(..))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V (toList)
import Prelude ()
import Prelude.Compat


data PState = PState { pLevel     :: Int
                     , pIndent    :: Builder
                     , pNewline   :: Builder
                     , pItemSep   :: Builder
                     , pKeyValSep :: Builder
                     , pNumFormat :: NumberFormat
                     , pSort      :: [(Text, Value)] -> [(Text, Value)]
                     }

-- | Indentation per level of nesting. @'Spaces' 0@ removes __all__ whitespace
--   from the output.
data Indent = Spaces Int | Tab

data NumberFormat
  -- | For numbers with absolute value less than 1e19, this follows the standard
  -- behaviour of the 'Data.Aeson.encode' function. Uses integer literals for
  -- integers (1, 2, 3...), simple decimals for fractional values between 0.1
  -- and 9,999,999, and scientific notation otherwise. For numbers with an
  -- absolute value larger than 1e19, always uses scientific notation.
  = Generic
  -- | Scientific notation (e.g. 2.3e123).
  | Scientific
  -- | Standard decimal notation
  | Decimal
  -- | Custom formatting function
  | Custom (S.Scientific -> Builder)

data Config = Config
    { confIndent  :: Indent
      -- ^ Indentation per level of nesting
    , confCompare :: Text -> Text -> Ordering
      -- ^ Function used to sort keys in objects
    , confNumFormat :: NumberFormat
    , confTrailingNewline :: Bool
      -- ^ Whether to add a trailing newline to the output
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
--  not sort objects by key, do not add trailing newline.
--
--  > defConfig = Config { confIndent = Spaces 4, confCompare = mempty, confNumFormat = Generic, confTrailingNewline = False }
defConfig :: Config
defConfig =
  Config {confIndent = Spaces 4, confCompare = mempty, confNumFormat = Generic, confTrailingNewline = False}

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

-- |A variant of 'Aeson.encodeToTextBuilder' that takes an additional configuration
--  parameter.
encodePrettyToTextBuilder' :: ToJSON a => Config -> a -> Builder
encodePrettyToTextBuilder' Config{..} x = fromValue st (toJSON x) <> trail
  where
    st      = PState 0 indent newline itemSep kvSep confNumFormat sortFn
    indent  = case confIndent of
                Spaces n -> mconcat (replicate n " ")
                Tab      -> "\t"
    newline = case confIndent of
                Spaces 0 -> ""
                _        -> "\n"
    itemSep = ","
    kvSep   = case confIndent of
                Spaces 0 -> ":"
                _        -> ": "
    sortFn  = sortBy (confCompare `on` fst)
    trail   = if confTrailingNewline then "\n" else ""


fromValue :: PState -> Value -> Builder
fromValue st@PState{..} val = go val
  where
    go (Array v)  = fromCompound st ("[","]") fromValue (V.toList v)
    go (Object m) = fromCompound st ("{","}") fromPair (pSort (toList' m))
    go (Number x) = fromNumber st x
    go v          = Aeson.encodeToTextBuilder v

#if MIN_VERSION_aeson(2,0,0)
    toList' = fmap (\(k, v) -> (AK.toText k, v)) . AKM.toList
#else
    toList' = H.toList
#endif

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st@PState{..} (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else pNewline <> items' <> pNewline <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse (pItemSep <> pNewline) $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pLevel = pLevel + 1}

fromPair :: PState -> (Text, Value) -> Builder
fromPair st (k,v) =
  Aeson.encodeToTextBuilder (toJSON k) <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState{..} = mconcat (replicate pLevel pIndent)

fromNumber :: PState -> S.Scientific -> Builder
fromNumber st x = case pNumFormat st of
  Generic
    | (x > 1.0e19 || x < -1.0e19) -> formatScientificBuilder S.Exponent Nothing x
    | otherwise -> Aeson.encodeToTextBuilder $ Number x
  Scientific -> formatScientificBuilder S.Exponent Nothing x
  Decimal    -> formatScientificBuilder S.Fixed Nothing x
  Custom f   -> f x
