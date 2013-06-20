{-# LANGUAGE TemplateHaskell #-}
module Data.EDN.Test.QuickCheck.TH where

import           Control.Applicative   ((<$>), (<*>))
import           Language.Haskell.TH
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)



import qualified Data.EDN              as E
import qualified Test.QuickCheck       as QC

--------------------------------------------------------------------------------

serializeProperty :: (QC.Arbitrary a, Eq a, E.ToEDN a, E.FromEDN a) => a -> Bool
serializeProperty object = do
  case decoded of
    Just result -> result == object
    Nothing -> False
  where
      encoded = E.encode object
      decoded = E.decode encoded

getTestPropertyName :: Name -> Name
getTestPropertyName qualTypeName =
    mkName $ "test" ++ (nameBase qualTypeName) ++ "Serialization"

mkEDNSerializeSpecGroup :: [Name] -> Q [Dec]
mkEDNSerializeSpecGroup names = do
    let fnName = mkName "ednSerializationProperties"
    specType <- [t| Spec |]
    describeFn <- [| describe |]
    return $ [ SigD fnName specType
             , ValD (VarP fnName)
                    (NormalB (AppE (AppE describeFn
                                        (LitE (StringL "EDN.Serialization")))
                                   (DoE (map (NoBindS . VarE . getTestPropertyName)
                                             names)))) []]

mkSerializeSpec :: Name -> Q [Dec]
mkSerializeSpec qualTypeName = do
    let typeName = nameBase qualTypeName
        fnName = getTestPropertyName qualTypeName
    propFn <- [| prop |]
    serializePropertyFn <- [| serializeProperty |]
    boolType <- [t| Bool |]
    specType <- [t| Spec |]
    return [ SigD fnName specType
           , ValD (VarP fnName)
                    (NormalB (AppE
                      (AppE propFn
                        (LitE (StringL $ typeName ++ " serialization/deserialization")))
                      (SigE serializePropertyFn
                        (AppT (AppT ArrowT (ConT qualTypeName))
                              boolType)))) []]

mkSerializeSpecs :: [Name] -> Q [Dec]
mkSerializeSpecs types =
    (++) <$> (concat `fmap` mapM mkSerializeSpec types)
         <*> (mkEDNSerializeSpecGroup types)
