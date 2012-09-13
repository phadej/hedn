module Data.EDN.Types (
    -- * Types

    TaggedValue(..), Value(..),

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
    makeList, makeVec, makeSet, makeMap
) where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

-- | A \"raw\" EDN value represented as a Haskell value.
data Value = Nil
           | Boolean Bool
           | String Text
           | Character Char
           | Symbol ByteString ByteString
           | Keyword ByteString
           | Integer Integer
           | Floating Double
           | List [TaggedValue]
           | Vec (V.Vector TaggedValue)
           | Map (M.Map Value TaggedValue)
           | Set (S.Set TaggedValue)
           deriving (Eq, Ord, Show)

-- | A 'Value' wrapped into a namespaced tag.
data TaggedValue = NoTag Value
                 | Tagged Value ByteString ByteString
                 deriving (Eq, Ord, Show)

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

-- | Attach a namespaced tag to a 'Value'.
tag :: ByteString -> ByteString -> Value -> TaggedValue
tag ns t value = Tagged value ns t
{-# INLINE tag #-}

-- | Wrap a 'Value' into tagless container.
notag :: Value -> TaggedValue
notag = NoTag
{-# INLINE notag #-}

-- | Replace a tag on a value.
setTag :: ByteString -> ByteString -> TaggedValue -> TaggedValue
setTag ns t (NoTag v) = tag ns t v
setTag ns t (Tagged v _ _) = tag ns t v
{-# INLINE setTag #-}

-- | Extract namespace and tag from a tagged container. Will be a pair of 'BS.empty' for tagless containers.
getTag :: TaggedValue -> (ByteString, ByteString)
getTag (NoTag _) = (BS.empty, BS.empty)
getTag (Tagged _ ns t) = (ns, t)

-- | Extract bare value from a tagged or tagless container.
stripTag :: TaggedValue -> Value
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

-- | Create an EDN 'Map' from a assoc list with untagged keys and tagged values.
makeMap :: [(Value, TaggedValue)] -> Value
makeMap = Map . M.fromList
{-# INLINE makeMap #-}
