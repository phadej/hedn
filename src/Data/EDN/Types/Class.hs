{-# LANGUAGE OverloadedStrings, FlexibleInstances, IncoherentInstances #-}

module Data.EDN.Types.Class (
    ToEDN, FromEDN, toEDN, fromEDN, fromEDNv, (.:), (.:?)
) where

import Control.Applicative (pure, (<$>))
import Control.Monad (liftM, liftM2)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Parser as P
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

instance (ToEDN a) => ToEDN (E.Tagged a) where
    toEDN (E.Tagged v ns t) = E.setTag ns t $ toEDN v
    toEDN (E.NoTag v) = toEDN v
    {-# INLINE toEDN #-}

instance (FromEDN a) => FromEDN (E.Tagged a) where
   parseEDN (E.Tagged v ns t) = E.tag ns t <$> parseEDNv v
   parseEDN (E.NoTag v) = E.notag <$> parseEDNv v
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

instance (Ord a, ToEDN a) => ToEDN (S.Set a) where
    toEDNv = E.Set . S.map toEDN
    {-# INLINE toEDNv #-}

instance (Ord a, FromEDN a) => FromEDN (S.Set a) where
    parseEDNv (E.Set s) = mapMset parseEDN s
    parseEDNv v = typeMismatch "Set" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b) => ToEDN (M.Map a b) where
    toEDNv m = E.Map $! M.fromList [(toEDNv k, toEDN v) | (k, v) <- M.assocs m]
    {-# INLINE toEDNv #-}

instance (Ord a, FromEDN a, FromEDN b) => FromEDN (M.Map a b) where
    parseEDNv (E.Map m) = mapMmap parseEDNv parseEDN m
    parseEDNv v = typeMismatch "Map" v
    {-# INLINE parseEDNv #-}

instance ToEDN E.Value where
    toEDNv = id

instance FromEDN E.Value where
    parseEDNv = pure

instance ToEDN E.TaggedValue where
    toEDN = id

instance FromEDN E.TaggedValue where
    parseEDN = pure

-- | Convert a value from 'E.TaggedValue', failing if the types do not match.
fromEDN :: FromEDN a => E.TaggedValue -> P.Result a
fromEDN = P.parse parseEDN
{-# INLINE fromEDN #-}

-- | Convert a value from 'E.Value', failing if the types do not match.
fromEDNv :: FromEDN a => E.Value -> P.Result a
fromEDNv = P.parse parseEDNv
{-# INLINE fromEDNv #-}

-- | Retrieve the value associated with the given key of an 'E.EDNMap'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid. If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (Show k, ToEDN k, FromEDN a) => E.EDNMap -> k -> P.Parser a
emap .: key = case M.lookup (toEDNv key) emap of
                  Nothing -> fail $ "key " ++ show key ++ " not present"
                  Just v -> parseEDN v
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'E.EDNMap'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (ToEDN k, FromEDN a) => E.EDNMap -> k -> P.Parser (Maybe a)
emap .:? key = case M.lookup (toEDNv key) emap of
                   Nothing -> pure Nothing
                   Just v -> parseEDN v
{-# INLINE (.:?) #-}

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

mapMset :: (Monad m, Ord b) => (a -> m b) -> S.Set a -> m (S.Set b)
mapMset f s = mapM f (S.toList s) >>= return . S.fromList
{-# INLINE mapMset #-}

mapMmap :: (Ord a2, Monad m) => (a1 -> m a2) -> (b1 -> m b2) -> M.Map a1 b1 -> m (M.Map a2 b2)
mapMmap kf vf = liftM M.fromList . mapM (\(k, v) -> liftM2 (,) (kf k) (vf v)) . M.assocs
{-# INLINE mapMmap #-}
