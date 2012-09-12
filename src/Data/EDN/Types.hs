module Data.EDN.Types where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Hashable as H
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

data Value = Nil
           | Boolean Bool
           | String Text
           | Character Char
           | Symbol Int
           | Keyword ByteString
           | Integer Integer
           | Floating Double
           | List [Value]
           | Vec (V.Vector Value)
           | Map (M.Map Value Value)
           | Set (S.Set Value)
           deriving (Eq, Ord, Show)

data TaggedValue = NoTag { val :: Value }
                 | Tagged { val :: Value, prefix :: ByteString, tag :: ByteString } deriving (Eq, Show)

wrapTagged :: Maybe (ByteString, ByteString) -> Value -> TaggedValue
wrapTagged Nothing value              = NoTag value
wrapTagged (Just (prefix, tag)) value = Tagged value prefix tag

symbol :: ByteString -> ByteString -> Value
symbol ns key = Symbol (H.hash ns `H.combine` H.hash key)

makeVec :: [Value] -> Value
makeVec = Vec . V.fromList

makeSet :: [Value] -> Value
makeSet = Set . S.fromList

makeMap :: [(Value, Value)] -> Value
makeMap = Map . M.fromList
