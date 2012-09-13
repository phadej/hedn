module Data.EDN.Types.Class where

import Control.Applicative (pure, (<$>))

import Data.Parser as P
import qualified Data.EDN.Types as E

class ToEDN a where
    toEDN :: a -> E.TaggedValue

class FromEDN a where
    parseEDN :: E.TaggedValue -> P.Parser a

instance (ToEDN a) => ToEDN (Maybe a) where
    toEDN (Just a) = toEDN a
    toEDN Nothing = E.nil
    {-# INLINE toEDN #-}

instance (FromEDN a) => (FromEDN (Maybe a)) where
    parseEDN (E.NoTag E.Nil) = pure Nothing
    parseEDN (E.Tagged E.Nil _ _) = pure Nothing
    parseEDN a = Just <$> parseEDN a
    {-# INLINE parseEDN #-}

instance ToEDN Bool where
    toEDN = E.bool
    {-# INLINE toEDN #-}

instance FromEDN Bool where
    parseEDN (E.NoTag  (E.Boolean b))     = pure b
    parseEDN (E.Tagged (E.Boolean b) _ _) = pure b
    parseEDN v                            = typeMismatch "Boolean" v
    {-# INLINE parseEDN #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> E.TaggedValue  -- ^ The actual value encountered.
             -> P.Parser a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case E.stripTag actual of
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
