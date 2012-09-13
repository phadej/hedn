module Data.EDN.Types (
    -- * Types
    Value(..),
    TaggedValue(..),
    -- * Constructors
    makeVec,
    makeSet,
    makeMap
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
           | List [Value]
           | Vec (V.Vector Value)
           | Map (M.Map Value Value)
           | Set (S.Set Value)
           deriving (Eq, Ord, Show)

-- | A 'Value' wrapped into a namespaced tag.
data TaggedValue = NoTag Value
                 | Tagged Value ByteString ByteString
                 deriving (Eq, Show)

-- | Create an EDN Vector from a 'Value' list.
makeVec :: [Value] -> Value
makeVec = Vec . V.fromList
{-# INLINE makeVec #-}

-- | Create an EDN Set from a 'Value' list.
makeSet :: [Value] -> Value
makeSet = Set . S.fromList
{-# INLINE makeSet #-}

-- | Create an EDN Map from a 'Value' list.
makeMap :: [(Value, Value)] -> Value
makeMap = Map . M.fromList
{-# INLINE makeMap #-}
