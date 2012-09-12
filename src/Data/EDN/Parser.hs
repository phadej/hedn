{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Parser where

import Prelude hiding (String, takeWhile, dropWhile)
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator
import Control.Applicative (pure, (<|>), (*>))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BS

import Data.EDN.Types

isSpaceOrComma :: Char -> Bool
isSpaceOrComma ' ' = True
isSpaceOrComma '\r' = True
isSpaceOrComma '\t' = True
isSpaceOrComma ',' = True
isSpaceOrComma _ = False

spaceOrComma :: Parser Char
spaceOrComma = satisfy isSpaceOrComma <?> "space/comma"

parseNil :: Parser Value
parseNil = do
    skipSpace
    A.string "nil"
    return $! Nil

parseBool :: Parser Value
parseBool = choice [ string "true" *> pure (Boolean True)
           , string "false" *> pure (Boolean False)
           ]

parseString :: Parser Value
parseString = do
    skipSpace
    char '"'
    x <- takeWhile (/= '"')
    char '"'
    return $! String (decodeUtf8 x)

parseCharacter :: Parser Value
parseCharacter = do
    skipSpace
    char '\\'
    x <- string "newline" <|> string "space" <|> string "tab" <|> A.take 1
    return . Character $! case x of
                              "newline" -> '\n'
                              "space" -> ' '
                              "tab" -> '\t'
                              _ -> BS.head x

parseSymbol :: Parser Value
parseSymbol = do
    skipSpace
    c <- satisfy (inClass "a-zA-Z.*/!?+_-")
    x <- takeWhile (inClass "a-zA-Z0-9#:.*/!?+_-")
    return $! symbol "" (BS.cons c x)

parseKeyword :: Parser Value
parseKeyword = do
    skipSpace
    char ':'
    x <- takeWhile (inClass "a-zA-Z0-9.*/!?+_-")
    return $! Keyword x

parseInteger :: Parser Value
parseInteger = do
    skipSpace
    i <- decimal
    return $! Integer i

parseFloating :: Parser Value
parseFloating = do
    skipSpace
    f <- fromIntegral `fmap` decimal
    return $! Floating f

parseList :: Parser Value
parseList = do
    skipSpace
    char '('
    vs <- parseValue `sepBy` spaceOrComma
    char ')'
    return $! List vs

parseVector :: Parser Value
parseVector = do
    skipSpace
    char '['
    vs <- parseValue `sepBy` spaceOrComma
    char ']'
    return $! makeVec vs

parseMap :: Parser Value
parseMap = do
    skipSpace
    char '{'
    pairs <- parseAssoc `sepBy` spaceOrComma
    char '}'
    return $! makeMap pairs
    where
        parseAssoc = do
            key <- parseValue
            val <- parseValue
            return $! (key, val)

parseSet :: Parser Value
parseSet = do
    skipSpace
    char '#'
    char '{'
    vs <- parseValue `sepBy` spaceOrComma
    char '}'
    return $! makeSet vs

skipComment :: Parser ()
skipComment = skipSpace >> char ';' >> skipWhile (/= '\n')

parseValue :: Parser Value
parseValue = skipSpace >> parseSet <|> parseMap
          <|> parseVector <|> parseList
          <|> parseNil <|> parseBool
          <|> parseInteger <|> parseFloating
          <|> parseKeyword <|> parseSymbol
          <|> parseCharacter
          <|> parseString

parseTag :: Parser (Maybe (BS.ByteString, BS.ByteString))
parseTag = do
    skipSpace
    withNS <|> withoutNS <|> noTag
    where
        withNS = do
            char '#'
            ns <- takeWhile (inClass "a-zA-Z0-9-")
            char '/'
            tag <- takeWhile (inClass "a-zA-Z0-9-")
            return $! Just (ns, tag)
        withoutNS = do
            char '#'
            tag <- takeWhile (inClass "a-zA-Z0-9-")
            return $! Just ("", tag)
        noTag = do
            return $! Nothing


parseTagged :: Parser TaggedValue
parseTagged = do
    tag <- parseTag
    val <- parseValue
    return $! wrapTagged tag val
