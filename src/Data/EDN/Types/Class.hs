{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Data.EDN.Types.Class where

import Control.Applicative (pure, (<$>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V

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

instance ToEDN () where
    toEDNv _ = E.List []
    {-# INLINE toEDNv #-}

instance FromEDN () where
    parseEDNv (E.List l) | null l = pure ()
    parseEDNv v = typeMismatch "()" v
    {-# INLINE parseEDNv #-}

instance ToEDN [Char] where
    toEDNv = E.String . T.pack
    {-# INLINE toEDNv #-}

instance FromEDN [Char] where
    parseEDNv (E.String t) = pure $ T.unpack t
    parseEDNv (E.Symbol "" s) = pure $ BS.unpack s
    parseEDNv (E.Symbol ns s) = pure . BS.unpack $ BS.concat [ns, "/", s]
    parseEDNv (E.Keyword k) = pure . BS.unpack $ BS.cons ':' k
    parseEDNv v = typeMismatch "String/Symbol/Keyword" v
    {-# INLINE parseEDNv #-}

instance ToEDN T.Text where
    toEDNv = E.String
    {-# INLINE toEDNv #-}

instance FromEDN T.Text where
    parseEDNv (E.String t) = pure t
    parseEDNv v = typeMismatch "String" v
    {-# INLINE parseEDNv #-}

instance ToEDN TL.Text where
    toEDNv = E.String . TL.toStrict
    {-# INLINE toEDNv #-}

instance FromEDN TL.Text where
    parseEDNv (E.String t) = pure $ TL.fromStrict t
    parseEDNv v = typeMismatch "String" v
    {-# INLINE parseEDNv #-}

instance ToEDN BS.ByteString where
    toEDNv = E.String . TE.decodeUtf8
    {-# INLINE toEDNv #-}

instance FromEDN BS.ByteString where
    parseEDNv (E.String t) = pure $ TE.encodeUtf8 t
    parseEDNv v = typeMismatch "String" v
    {-# INLINE parseEDNv #-}

instance ToEDN BSL.ByteString where
    toEDNv = E.String . TL.toStrict . TLE.decodeUtf8
    {-# INLINE toEDNv #-}

instance FromEDN BSL.ByteString where
    parseEDNv (E.String t) = pure . TLE.encodeUtf8 . TL.fromStrict $ t
    parseEDNv v = typeMismatch "String" v
    {-# INLINE parseEDNv #-}

instance ToEDN Char where
    toEDNv = E.Character
    {-# INLINE toEDNv #-}

instance FromEDN Char where
    parseEDNv (E.Character c) = pure $ c
    parseEDNv v = typeMismatch "Character" v
    {-# INLINE parseEDNv #-}

instance ToEDN Double where
    toEDNv = E.Floating
    {-# INLINE toEDNv #-}

instance FromEDN Double where
    parseEDNv (E.Floating d) = pure d
    parseEDNv v = typeMismatch "Floating" v
    {-# INLINE parseEDNv #-}

instance ToEDN Integer where
    toEDNv = E.Integer
    {-# INLINE toEDNv #-}

instance FromEDN Integer where
    parseEDNv (E.Integer i) = pure i
    parseEDNv v = typeMismatch "Integer" v
    {-# INLINE parseEDNv #-}

instance ToEDN a => ToEDN [a] where
    toEDNv = E.List . map toEDN
    {-# INLINE toEDNv #-}

instance FromEDN a => FromEDN [a] where
    parseEDNv (E.List vs) = mapM parseEDN vs
    parseEDNv v = typeMismatch "List" v
    {-# INLINE parseEDNv #-}

instance ToEDN a => ToEDN (V.Vector a) where
    toEDNv = E.Vec . V.map toEDN
    {-# INLINE toEDNv #-}

instance FromEDN a => FromEDN (V.Vector a) where
    parseEDNv (E.Vec as) = V.mapM parseEDN as
    parseEDNv v = typeMismatch "Vec" v
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
