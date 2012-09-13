{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Encode (fromValue, fromTagged, encode) where

import Data.Monoid (mappend)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.EDN.Types as E

-- | Encode a Tagged EDN value to a 'Builder'.
fromTagged :: E.TaggedValue -> Builder
fromTagged (E.NoTag v) = fromValue v
fromTagged (E.Tagged v "" t) = singleton '#' <> string t <> singleton ' ' <> fromValue v
fromTagged (E.Tagged v ns t) = singleton '#' <> string ns <> singleton '/' <> string t <> singleton ' ' <> fromValue v

-- | Encode a raw EDN value to a 'Builder'.
fromValue :: E.Value -> Builder

fromValue E.Nil = "nil"

fromValue (E.Boolean b) = if b then "true" else "false"

fromValue (E.String t) = singleton '"' <> quote t <> singleton '"'

fromValue (E.Character c) = singleton '\\' <> singleton c

fromValue (E.Symbol "" v) = string v
fromValue (E.Symbol ns v) = string ns <> singleton '/' <> string v

fromValue (E.Keyword kw) = singleton ':' <> string kw

fromValue (E.Integer i) = decimal i

fromValue (E.Floating f) = realFloat f

fromValue (E.List xs) = singleton '(' <> fromList xs <> singleton ')'

fromValue (E.Vec xs) = singleton '[' <> fromList (V.toList xs) <> singleton ']'

fromValue (E.Set xs) = "#{" <> fromList (S.toList xs) <> singleton '}'

fromValue (E.Map as) = singleton '{' <> fromAssoc (M.assocs as) <> singleton '}'

string :: BS.ByteString -> Builder
string s = fromLazyText . decodeUtf8 . L.fromChunks $ [s]

quote :: T.Text -> Builder
quote q = case T.uncons t of
    Nothing -> fromText h
    Just (c, t') -> fromText h <> escape c <> quote t'
    where
        (h, t) = T.break isEscape q
        isEscape c = c == '\"' || c == '\\' || c < '\x20'
        escape '\"' = "\\\""
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape c = singleton c

fromList :: [E.TaggedValue] -> Builder
fromList [] = ""
fromList (x:[]) = fromTagged x
fromList (x:xs) = fromTagged x <> singleton ' ' <> fromList xs

fromAssoc :: [(E.Value, E.TaggedValue)] -> Builder
fromAssoc [] = ""
fromAssoc ((k, v):[]) = fromValue k <> singleton ' ' <> fromTagged v
fromAssoc ((k, v):as) = fromValue k <> singleton ' ' <> fromTagged v <> singleton ' ' <> fromAssoc as

-- | Serialize a EDN value as a lazy 'L.ByteString'.
encode :: E.TaggedValue -> L.ByteString
encode = encodeUtf8 . toLazyText . fromTagged
{-# INLINE encode #-}

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
