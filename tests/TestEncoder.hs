{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.EDN.Types as E
import Data.EDN.Encode (fromValue)

main = do
    print . fromValue $ E.List [E.Keyword "a", E.Keyword "b", E.Integer 42]
