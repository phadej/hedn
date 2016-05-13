{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings   #-}
module Data.EDN.Types.Class (
    -- * Type conversion
    ToEDN(..), FromEDN(..), fromEDN, fromEDNv,

    -- * EDN value decoding
    decode, eitherDecode, DP.parse, DP.parseEither, DP.parseMaybe, DP.Parser, DP.Result(..),

    -- * Convenience functions
    (.=), (.:), (.:?), (.!=), typeMismatch
) where

import Prelude ()
import Prelude.Compat

import           Control.Monad              (liftM, liftM2)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First (..))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.Time.Clock            (UTCTime)
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format           (formatTime, parseTimeM)
#else
import           Data.Time.Format           (formatTime, parseTime)
#endif
import           Data.Time.Locale.Compat    (defaultTimeLocale)
import qualified Data.Vector                as V

import qualified Data.EDN.Parser            as P
import qualified Data.EDN.Types             as E
import           Data.Parser                (Parser, Result)
import qualified Data.Parser                as DP

-- | A type that can be converted to JSON.
class ToEDN a where
    toEDN :: a -> E.TaggedValue
    toEDN = E.notag . toEDNv
    {-# INLINE toEDN #-}

    toEDNv :: a -> E.Value
    toEDNv = E.stripTag . toEDN
    {-# INLINE toEDNv #-}

-- | A type that can be converted from EDN, with a possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if an 'M.Map' is missing a required key, or
-- the value is of the wrong type.
class FromEDN a where
    parseEDN :: E.TaggedValue -> Parser a
    parseEDN = parseEDNv . E.stripTag
    {-# INLINE parseEDN #-}

    parseEDNv :: E.Value -> Parser a
    parseEDNv = parseEDN . E.notag
    {-# INLINE parseEDNv #-}

instance (ToEDN a) => ToEDN (Maybe a) where
    toEDN (Just a) = toEDN a
    toEDN Nothing = E.nil
    {-# INLINE toEDN #-}

instance (FromEDN a) => (FromEDN (Maybe a)) where
    parseEDN (E.NoTag E.Nil) = pure Nothing
    parseEDN a = Just <$> parseEDN a
    {-# INLINE parseEDN #-}

instance (ToEDN a, ToEDN b) => ToEDN (Either a b) where
    toEDN (Left a) = E.tag "either" "left" $ toEDNv a
    toEDN (Right b) = E.tag "either" "right" $ toEDNv b
    {-# INLINE toEDN #-}

instance (FromEDN a, FromEDN b) => FromEDN (Either a b) where
    parseEDN (E.Tagged v "either" "left") = Left <$> parseEDNv v
    parseEDN (E.Tagged v "either" "right") = Right <$> parseEDNv v
    parseEDN (E.Tagged {}) = fail "incorrect tag"
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
    parseEDNv (E.Character c) = pure c
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

instance ToEDN Int where
    toEDNv = E.Integer . fromIntegral
    {-# INLINE toEDNv #-}

instance FromEDN Int where
    parseEDNv (E.Integer i) = return (fromIntegral i)
    parseEDNv v = typeMismatch "Int" v


showRFC3339 :: UTCTime -> String
showRFC3339 time =
    concat [fm "%FT%T." time
           ,  take 3 $ fm "%-q" time
           ,  "+00:00"]
  where
    fm = formatTime defaultTimeLocale

instance ToEDN UTCTime where
    toEDN time = E.Tagged (E.String . T.pack $ showRFC3339 time)
                          ""
                          "inst"
    {-# INLINE toEDN #-}

instance FromEDN UTCTime where
    parseEDN val@(E.Tagged (E.String ts) "" "inst") = do
        let result = getFirst . mconcat $ map (First . parseTime') validRFC3339
        case result of
          Just time -> return time
          Nothing -> typeMismatch "UTCTime" $ E.stripTag val
      where
        tsStr = T.unpack ts
#if MIN_VERSION_time(1,5,0)
        parseTime' fmt = parseTimeM True defaultTimeLocale fmt tsStr
#else
        parseTime' fmt = parseTime defaultTimeLocale fmt tsStr
#endif
        validRFC3339 = [ "%FT%T%Q%z"
                       , "%FT%T%QZ"
                       , "%FT%T%z"
                       , "%FT%TZ" ]
    parseEDN v = typeMismatch "UTCTime" $ E.stripTag v
    {-# INLINE parseEDN #-}


instance ToEDN a => ToEDN [a] where
    toEDNv = E.List . map toEDN
    {-# INLINE toEDNv #-}

instance FromEDN a => FromEDN [a] where
    parseEDNv (E.Vec vs) = V.toList <$> V.mapM parseEDN vs
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

instance (ToEDN a, ToEDN b) => ToEDN (a, b) where
    toEDNv (a, b) = E.Vec $! V.fromList [toEDN a, toEDN b]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b) => FromEDN (a, b) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 2 = (,) <$> parseEDN (vec V.! 0)
                                <*> parseEDN (vec V.! 1)
      | otherwise = typeMismatch "(a, b)" v
    parseEDNv v = typeMismatch "(a, b)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c) => ToEDN (a, b, c) where
    toEDNv (a, b, c) = E.Vec $! V.fromList [toEDN a, toEDN b, toEDN c]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c) => FromEDN (a, b, c) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 3 = (,,) <$> parseEDN (vec V.! 0)
                                 <*> parseEDN (vec V.! 1)
                                 <*> parseEDN (vec V.! 2)
      | otherwise = typeMismatch "(a, b, c)" v
    parseEDNv v = typeMismatch "(a, b, c)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d) => ToEDN (a, b, c, d) where
    toEDNv (a, b, c, d) = E.Vec $! V.fromList [toEDN a, toEDN b, toEDN c, toEDN d]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d) => FromEDN (a, b, c, d) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 4 = (,,,) <$> parseEDN (vec V.! 0)
                                  <*> parseEDN (vec V.! 1)
                                  <*> parseEDN (vec V.! 2)
                                  <*> parseEDN (vec V.! 3)
      | otherwise = typeMismatch "(a, b, c, d)" v
    parseEDNv v = typeMismatch "(a, b, c, d)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d, ToEDN e) => ToEDN (a, b, c, d, e) where
    toEDNv (a, b, c, d, e) = E.Vec $! V.fromList [
                               toEDN a
                             , toEDN b
                             , toEDN c
                             , toEDN d
                             , toEDN e ]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d, FromEDN e)
    => FromEDN (a, b, c, d, e) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 5 = (,,,,) <$> parseEDN (vec V.! 0)
                                   <*> parseEDN (vec V.! 1)
                                   <*> parseEDN (vec V.! 2)
                                   <*> parseEDN (vec V.! 3)
                                   <*> parseEDN (vec V.! 4)
      | otherwise = typeMismatch "(a, b, c, d, e)" v
    parseEDNv v = typeMismatch "(a, b, c, d, e)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d, ToEDN e, ToEDN f)
    => ToEDN (a, b, c, d, e, f) where
    toEDNv (a, b, c, d, e, f) = E.Vec $! V.fromList [
                               toEDN a
                             , toEDN b
                             , toEDN c
                             , toEDN d
                             , toEDN e
                             , toEDN f]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d, FromEDN e, FromEDN f)
    => FromEDN (a, b, c, d, e, f) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 6 = (,,,,,) <$> parseEDN (vec V.! 0)
                                    <*> parseEDN (vec V.! 1)
                                    <*> parseEDN (vec V.! 2)
                                    <*> parseEDN (vec V.! 3)
                                    <*> parseEDN (vec V.! 4)
                                    <*> parseEDN (vec V.! 5)
      | otherwise = typeMismatch "(a, b, c, d, e, f)" v
    parseEDNv v = typeMismatch "(a, b, c, d, e, f)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d, ToEDN e, ToEDN f, ToEDN g)
    => ToEDN (a, b, c, d, e, f, g) where
    toEDNv (a, b, c, d, e, f, g) =
        E.Vec $! V.fromList [ toEDN a
                            , toEDN b
                            , toEDN c
                            , toEDN d
                            , toEDN e
                            , toEDN f
                            , toEDN g]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d, FromEDN e, FromEDN f, FromEDN g)
    => FromEDN (a, b, c, d, e, f, g) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 7 = (,,,,,,) <$> parseEDN (vec V.! 0)
                                     <*> parseEDN (vec V.! 1)
                                     <*> parseEDN (vec V.! 2)
                                     <*> parseEDN (vec V.! 3)
                                     <*> parseEDN (vec V.! 4)
                                     <*> parseEDN (vec V.! 5)
                                     <*> parseEDN (vec V.! 6)
      | otherwise = typeMismatch "(a, b, c, d, e, f, g)" v
    parseEDNv v = typeMismatch "(a, b, c, d, e, f, g)" v
    {-# INLINE parseEDNv #-}

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d, ToEDN e, ToEDN f, ToEDN g, ToEDN h)
    => ToEDN (a, b, c, d, e, f, g, h) where
    toEDNv (a, b, c, d, e, f, g, h) =
        E.Vec $! V.fromList [ toEDN a
                            , toEDN b
                            , toEDN c
                            , toEDN d
                            , toEDN e
                            , toEDN f
                            , toEDN g
                            , toEDN h]
    {-# INLINE toEDNv #-}

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d,
          FromEDN e, FromEDN f, FromEDN g, FromEDN h)
    => FromEDN (a, b, c, d, e, f, g, h) where
    parseEDNv v@(E.Vec vec)
      | V.length vec == 8 = (,,,,,,,) <$> parseEDN (vec V.! 0)
                                      <*> parseEDN (vec V.! 1)
                                      <*> parseEDN (vec V.! 2)
                                      <*> parseEDN (vec V.! 3)
                                      <*> parseEDN (vec V.! 4)
                                      <*> parseEDN (vec V.! 5)
                                      <*> parseEDN (vec V.! 6)
                                      <*> parseEDN (vec V.! 7)
      | otherwise = typeMismatch "(a, b, c, d, e, f, g, h)" v
    parseEDNv v = typeMismatch "(a, b, c, d, e, f, g, h)" v
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
fromEDN :: FromEDN a => E.TaggedValue -> Result a
fromEDN = DP.parse parseEDN
{-# INLINE fromEDN #-}

-- | Convert a value from 'E.Value', failing if the types do not match.
fromEDNv :: FromEDN a => E.Value -> Result a
fromEDNv = DP.parse parseEDNv
{-# INLINE fromEDNv #-}

-- | Deserializes a EDN value from a lazy 'BSL.ByteString'.
-- If this fails to to incomplete or invalid input, 'Nothing' is returned.
decode :: FromEDN a => BSL.ByteString -> Maybe a
decode s = case P.parseMaybe s of
    Just tv -> DP.parseMaybe parseEDN tv
    Nothing -> Nothing

-- | Deserializes a EDN value from a lazy 'BSL.ByteString'.
-- If this fails to to incomplete or invalid input, 'Left' is returned
-- with an error message.
eitherDecode :: FromEDN a => BSL.ByteString -> Either String a
eitherDecode s = case P.parseEither s of
   Right tv -> DP.parseEither parseEDN tv
   Left e -> Left e

-- | Construct a 'Pair' from a key (as EDN keyword) and a value.
(.=) :: ToEDN a => BS.ByteString -> a -> E.Pair
name .= value = (E.Keyword name, toEDN value)
{-# INLINE (.=) #-}

-- | Retrieve the value associated with the given key of an 'E.EDNMap'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid. If the key and value are
-- optional, use '(.:?)' instead.
(.:) :: (Show k, ToEDN k, FromEDN a) => E.EDNMap -> k -> Parser a
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
(.:?) :: (ToEDN k, FromEDN a) => E.EDNMap -> k -> Parser (Maybe a)
emap .:? key = case M.lookup (toEDNv key) emap of
                   Nothing -> pure Nothing
                   Just v -> parseEDN v
{-# INLINE (.:?) #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '(.:)' instead.
--
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> E.Value -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case actual of
        E.Nil           -> "Nil"
        E.Boolean   _   -> "Boolean"
        E.String    _   -> "String"
        E.Character _   -> "Character"
        E.Symbol    _ _ -> "Symbol"
        E.Keyword   _   -> "Keyword"
        E.Integer   _   -> "Integer"
        E.Floating  _   -> "Floating"
        E.List      _   -> "List"
        E.Vec       _   -> "Vec"
        E.Map       _   -> "Map"
        E.Set       _   -> "Set"

mapMset :: (Applicative m, Monad m, Ord b)
        => (a -> m b)
        -> S.Set a
        -> m (S.Set b)
mapMset f s = S.fromList <$> traverse f (S.toList s)
{-# INLINE mapMset #-}

mapMmap :: (Ord a2, Monad m)
        => (a1 -> m a2)
        -> (b1 -> m b2)
        -> M.Map a1 b1
        -> m (M.Map a2 b2)
mapMmap kf vf = liftM M.fromList . mapM (\(k, v) -> liftM2 (,) (kf k) (vf v)) . M.assocs
{-# INLINE mapMmap #-}
