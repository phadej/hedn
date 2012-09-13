{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Encode where

import Data.Monoid (mappend)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as L

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

import Data.EDN.Types as E

fromValue :: Value -> Builder

fromValue E.Nil = "nil"

fromValue (E.Boolean b) = if b then "true" else "false"

-- FIXME: escape \t \r \\
fromValue (E.String t) = fromText t

fromValue (E.Character c) = singleton '\\' <> singleton c

fromValue (E.Symbol "" v) = string v
fromValue (E.Symbol ns v) = string ns <> singleton '/' <> string v

fromValue (E.Keyword kw) = singleton ':' <> string kw

fromValue (E.Integer i) = decimal i

fromValue (E.Floating f) = realFloat f

fromValue (E.List []) = "()"
fromValue (E.List xs) = singleton '(' <> fromList xs <> singleton ')'

fromValue (E.Vec xs)
    | V.null xs = "[]"
    | otherwise = singleton '[' <> fromList (V.toList xs) <> singleton ']'

fromValue (E.Set xs)
    | S.null xs = "#{}"
    | otherwise = "#{" <> fromList (S.toList xs) <> singleton '}'

fromValue (E.Map as)
    | M.null as = "{}"
    | otherwise = singleton '{' <> fromAssoc (M.assocs as) <> singleton '}'

string s = fromLazyText . decodeUtf8 . L.fromChunks $ [s]

fromList (x:[]) = fromValue x
fromList (x:xs) = fromValue x <> singleton ' ' <> fromList xs

fromAssoc ((k, v):[]) = fromValue k <> singleton ' ' <> fromValue v
fromAssoc ((k, v):as) = fromValue k <> singleton ' ' <> fromValue v <> singleton ' ' <> fromAssoc as

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
