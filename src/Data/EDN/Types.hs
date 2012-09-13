module Data.EDN.Types (
    -- * Types

    TaggedValue(..), Value(..),

    -- * Constructors

    -- ** Empty-tagged containers
    makeList, makeVec, makeSet, makeMap,

    -- ** Tagged containers
    makeVec', makeSet', makeMap'
) where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
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

-- | Create an EDN 'List' from a 'Value' list wrapping them into empty tags.
makeList :: [Value] -> Value
makeList = List . map NoTag
{-# INLINE makeList #-}

-- | Create an EDN 'Vector' from a 'TaggedValue' list.
makeVec' :: [TaggedValue] -> Value
makeVec' = Vec . V.fromList
{-# INLINE makeVec' #-}

-- | Create an EDN 'Vector' from a 'Value' list wrapping them into empty tags.
makeVec :: [Value] -> Value
makeVec = makeVec' . map NoTag
{-# INLINE makeVec #-}

-- | Create an EDN 'Set' from a 'TaggedValue' list.
makeSet' :: [TaggedValue] -> Value
makeSet' = Set . S.fromList
{-# INLINE makeSet' #-}

-- | Create an EDN 'Set' from a 'Value' list wrapping them into empty tags.
makeSet :: [Value] -> Value
makeSet = makeSet' . map NoTag
{-# INLINE makeSet #-}

-- | Create an EDN 'Map' from a assoc list with untagged keys and tagged values.
makeMap' :: [(Value, TaggedValue)] -> Value
makeMap' = Map . M.fromList
{-# INLINE makeMap' #-}

-- | Create an EDN 'Map' from a assoc list with untagged keys and values, wrapping values into empty tags.
makeMap :: [(Value, Value)] -> Value
makeMap as = makeMap' [(k, NoTag v) | (k, v) <- as]
{-# INLINE makeMap #-}