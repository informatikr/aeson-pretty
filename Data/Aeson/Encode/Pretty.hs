{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
    keyOrder,
    -- | Sort an array of Values with
    -- Null < Bool < Number < String < Array < Object.
    -- Objects are compared by considering them as [[(Text, Value)]]
    mkBasicValueCompare
) where

import Data.Aeson (Value(..), ToJSON(..), Object)
import qualified Data.Aeson.Encode as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import qualified Data.HashMap.Strict as H (toList)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Scientific as S (Scientific, FPFormat(..))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Prelude ()
import Prelude.Compat

type KeySorter = [(Text, Value)] -> [(Text, Value)]
type ListSorter = [Value] -> [Value]


data PState = PState { pLevel     :: Int
                     , pIndent    :: Builder
                     , pNewline   :: Builder
                     , pItemSep   :: Builder
                     , pKeyValSep :: Builder
                     , pNumFormat :: NumberFormat
                     , pSort      :: KeySorter
                     , pListSort  :: ListSorter
                     }

-- | Indentation per level of nesting. @'Spaces' 0@ removes __all__ whitespace
--   from the output.
data Indent = Spaces Int | Tab

data NumberFormat
  -- | The standard behaviour of the 'Aeson.encode' function. Uses
  --   integer literals for integers (1, 2, 3...), simple decimals
  --   for fractional values between 0.1 and 9,999,999, and scientific
  --   notation otherwise.
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
      -- ^ Flag to sort array of values
    , confValueCompare :: Value -> Value -> Ordering
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
--  > defConfig = Config { confIndent = Spaces 4
--                       , confCompare = mempty
--                       , confNumFormat = Generic
--                       , confValueCompare = mempty
--                       }
defConfig :: Config
defConfig =
  Config {confIndent = Spaces 4, confCompare = mempty
         , confNumFormat = Generic, confValueCompare = mempty}

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
    st      = PState 0 indent newline itemSep kvSep confNumFormat
                     sortFn arraySortFn
                     
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
                
    sortFn      = mkKeySorter confCompare
    arraySortFn = sortBy confValueCompare


fromValue :: PState -> Value -> Builder
fromValue st@PState{..} = go
  where
    go (Array v)  = fromCompound st ("[","]") fromValue (pListSort (V.toList v))
    go (Object m) = fromCompound st ("{","}") fromPair (pSort (H.toList m))
    go (Number x) = fromNumber st x
    go v          = Aeson.encodeToTextBuilder v

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
    st' = st { pLevel = pLevel + 1 }

fromPair :: PState -> (Text, Value) -> Builder
fromPair st (k,v) =
  Aeson.encodeToTextBuilder (toJSON k) <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState{..} = mconcat (replicate pLevel pIndent)

fromNumber :: PState -> S.Scientific -> Builder
fromNumber st x = case pNumFormat st of
  Generic    -> Aeson.encodeToTextBuilder $ Number x
  Scientific -> formatScientificBuilder S.Exponent Nothing x
  Decimal    -> formatScientificBuilder S.Fixed Nothing x
  Custom f   -> f x


data OrdValue = OrdValue KeySorter Value

instance Eq OrdValue where
  (OrdValue _ x) == (OrdValue _ y) = x == y


instance Ord OrdValue where
  compare (OrdValue _ Null) (OrdValue _ Null) = EQ
  compare (OrdValue _ Null) _                 = LT
  compare _                 (OrdValue _ Null) = GT
  
  compare (OrdValue _ (Bool x)) (OrdValue _ (Bool y)) = compare x y
  compare (OrdValue _ (Bool _)) _                     = LT
  compare _                     (OrdValue _ (Bool _)) = GT

  compare (OrdValue _ (Number x)) (OrdValue _ (Number y)) = compare x y
  compare (OrdValue _ (Number _)) _                       = LT
  compare _                       (OrdValue _ (Number _)) = GT

  compare (OrdValue _ (String x)) (OrdValue _ (String y)) = compare x y
  compare (OrdValue _ (String _)) _                       = LT
  compare _                       (OrdValue _ (String _)) = GT

  compare (OrdValue ks (Array x)) (OrdValue _ (Array y)) =
    compare (fmap (OrdValue ks) x) (fmap (OrdValue ks) y)
    
  compare (OrdValue _  (Array _)) _                      = LT
  compare _                       (OrdValue _ (Array _)) = GT

  compare (OrdValue ks (Object x)) (OrdValue _ (Object y)) =
    compare (toListOrderedByKeys ks x) (toListOrderedByKeys ks y)


toListOrderedByKeys :: KeySorter -> Object -> [(Text, OrdValue)]
toListOrderedByKeys ks obj =
  map (\(k, v) -> (k, OrdValue ks v)) $ ks $ H.toList obj

mkKeySorter :: (Text -> Text -> Ordering) -> KeySorter
mkKeySorter txtCompare = sortBy (txtCompare `on` fst)

mkBasicValueCompare ::  (Text -> Text -> Ordering) -> Value -> Value -> Ordering
mkBasicValueCompare txtCompare x y = compare (OrdValue ks x) (OrdValue ks y)
  where
    ks = mkKeySorter txtCompare

