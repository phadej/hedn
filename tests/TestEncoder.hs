{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.EDN.Types as E
import Data.EDN.Encode (encode)

main = do
    print . encode . E.NoTag . E.List $ map E.NoTag [E.Keyword "a", E.Keyword "b", E.Integer 42]
