{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Types.Class where

import Control.Applicative (pure, (<$>))

import Data.Parser as P
import qualified Data.EDN.Types as E

class ToEDN a where
    toEDN :: a -> E.TaggedValue
    toEDN = E.notag . toEDNv
    {-# INLINE toEDN #-}

    toEDNv :: a -> E.Value
    toEDNv = E.stripTag . toEDN
    {-# INLINE toEDNv #-}

class FromEDN a where
    parseEDN :: E.TaggedValue -> P.Parser a
    parseEDN = parseEDNv . E.stripTag
    {-# INLINE parseEDN #-}

    parseEDNv :: E.Value -> P.Parser a
    parseEDNv = parseEDN . E.notag
    {-# INLINE parseEDNv #-}

instance (ToEDN a) => ToEDN (Maybe a) where
    toEDN (Just a) = toEDN a
    toEDN Nothing = E.nil
    {-# INLINE toEDN #-}

instance (FromEDN a) => (FromEDN (Maybe a)) where
    parseEDNv E.Nil = pure Nothing
    parseEDNv a = Just <$> parseEDNv a
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b) => ToEDN (Either a b) where
    toEDN (Left a) = E.tag "either" "left" $ toEDNv a
    toEDN (Right b) = E.tag "either" "right" $ toEDNv b
    {-# INLINE toEDN #-}

instance (FromEDN a, FromEDN b) => FromEDN (Either a b) where
    parseEDN (E.Tagged v "either" "left") = Left <$> parseEDNv v
    parseEDN (E.Tagged v "either" "right") = Right <$> parseEDNv v
    parseEDN (E.Tagged _ _ _) = fail "incorrect tag"
    parseEDN (E.NoTag _) = fail "no tag"
    {-# INLINE parseEDN #-}

instance ToEDN Bool where
    toEDN = E.bool
    {-# INLINE toEDN #-}

instance FromEDN Bool where
    parseEDNv (E.Boolean b) = pure b
    parseEDNv v             = typeMismatch "Boolean" v
    {-# INLINE parseEDNv #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> E.Value -- ^ The actual value encountered.
             -> P.Parser a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case actual of
        E.Nil -> "Nil"
        E.Boolean _ -> "Boolean"
        E.String _ -> "String"
        E.Character _ -> "Character"
        E.Symbol _ _ -> "Symbol"
        E.Keyword _ -> "Keyword"
        E.Integer _ -> "Integer"
        E.Floating _ -> "Floating"
        E.List _ -> "List"
        E.Vec _ -> "Vec"
        E.Map _ -> "Map"
        E.Set _ -> "Set"
