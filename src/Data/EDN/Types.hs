{-# LANGUAGE FlexibleInstances #-}

module Data.EDN.Types (
    -- * Types
    Value(..), Tagged(..), TaggedValue,

    -- ** Internal containers
    EDNList, EDNVec, EDNSet, EDNMap, Pair,

    -- * Tag manipulation
    setTag, getTag, stripTag,

    -- * Constructors
    tag, notag,

    -- ** Trivial values
    nil,
    bool, true, false,
    char, string,
    symbol, symbolNS, keyword,
    integer, floating,

    -- ** Containers
    makeList, makeVec, makeSet, makeMap, (.=)
) where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

-- | Abstract namespaced tag.
data Tagged a = NoTag !a
              | Tagged !a !ByteString !ByteString
              deriving (Eq, Ord, Show)

type TaggedValue = Tagged Value

type EDNList = [TaggedValue]
type EDNVec = V.Vector TaggedValue
type EDNMap = M.Map Value TaggedValue
type EDNSet = S.Set TaggedValue

-- | A \"raw\" EDN value represented as a Haskell value.
data Value = Nil
           | Boolean !Bool
           | String !Text
           | Character !Char
           | Symbol !ByteString !ByteString
           | Keyword !ByteString
           | Integer !Integer
           | Floating !Double
           | List EDNList
           | Vec !EDNVec
           | Map !EDNMap
           | Set !EDNSet
           deriving (Eq, Ord, Show)

-- | Strings starting with \":\" will become keywords.
instance IsString Value where
  fromString (':':s) = Keyword . BS.pack $ s
  fromString s = String . T.pack $ s
  {-# INLINE fromString #-}

-- | Strings will become an tagless EDN strings.
instance IsString (Tagged Value) where
  fromString = string . T.pack
  {-# INLINE fromString #-}

-- | Basic EDN nil.
nil :: TaggedValue
nil = NoTag Nil
{-# INLINE nil #-}

-- | Basic EDN boolean.
bool :: Bool -> TaggedValue
bool = NoTag . Boolean
{-# INLINE bool #-}

-- | Const EDN True.
true :: TaggedValue
true = bool True
{-# INLINE true #-}

-- | Const EDN False.
false :: TaggedValue
false = bool False
{-# INLINE false #-}

-- | Basic EDN character.
char :: Char -> TaggedValue
char = NoTag . Character
{-# INLINE char #-}

-- | Basic EDN string.
string :: Text -> TaggedValue
string = NoTag . String
{-# INLINE string #-}

-- | A namespaced symbol.
symbolNS :: ByteString -> ByteString -> TaggedValue
symbolNS ns value = NoTag $ Symbol ns value
{-# INLINE symbolNS #-}

-- | \"Bare\" symbol.
symbol :: ByteString -> TaggedValue
symbol = symbolNS BS.empty
{-# INLINE symbol #-}

-- | Basic EDN keyword.
keyword :: ByteString -> TaggedValue
keyword = NoTag . Keyword
{-# INLINE keyword #-}

-- | Basic EDN integer.
integer :: Integer -> TaggedValue
integer = NoTag . Integer
{-# INLINE integer #-}

-- | Basic EDN fp number.
floating :: Double -> TaggedValue
floating = NoTag . Floating
{-# INLINE floating #-}

-- | Attach a namespaced tag to a value.
tag :: ByteString -> ByteString -> a -> Tagged a
tag ns t value = Tagged value ns t
{-# INLINE tag #-}

-- | Wrap a value into tagless container.
notag :: a -> Tagged a
notag = NoTag
{-# INLINE notag #-}

-- | Replace a tag on a 'Tagged' value.
setTag :: ByteString -> ByteString -> Tagged a -> Tagged a
setTag ns t (NoTag v) = tag ns t v
setTag ns t (Tagged v _ _) = tag ns t v
{-# INLINE setTag #-}

-- | Extract namespace and tag from a tagged container. Will be a pair of 'BS.empty' for tagless containers.
getTag :: TaggedValue -> (ByteString, ByteString)
getTag (NoTag _) = (BS.empty, BS.empty)
getTag (Tagged _ ns t) = (ns, t)

-- | Extract bare value from a tagged or tagless container.
stripTag :: Tagged a -> a
stripTag (NoTag v) = v
stripTag (Tagged v _ _) = v
{-# INLINE stripTag #-}


-- | Create an EDN 'List' from a 'Value' list wrapping them into empty tags.
makeList :: [TaggedValue] -> Value
makeList = List
{-# INLINE makeList #-}

-- | Create an EDN 'Vector' from a 'TaggedValue' list.
makeVec :: [TaggedValue] -> Value
makeVec = Vec . V.fromList
{-# INLINE makeVec #-}

-- | Create an EDN 'Set' from a 'TaggedValue' list.
makeSet :: [TaggedValue] -> Value
makeSet = Set . S.fromList
{-# INLINE makeSet #-}

-- | A key\/value pair for a EDN Map
type Pair = (Value, TaggedValue)

-- | Create an EDN 'Map' from a assoc list with untagged keys and tagged values.
makeMap :: [Pair] -> Value
makeMap = Map . M.fromList
{-# INLINE makeMap #-}

-- | Construct a 'Pair' from a key (as EDN keyword) and a value.
(.=) :: ByteString -> TaggedValue -> Pair
name .= value = (Keyword name, value)
{-# INLINE (.=) #-}
