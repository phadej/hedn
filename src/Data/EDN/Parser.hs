{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Parser where

import Prelude hiding (String, takeWhile, dropWhile)
import Data.Attoparsec.Char8 as A
import Data.Attoparsec.Combinator
import Control.Applicative (pure, (<|>), (*>))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Search (replace)

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

    x <- A.scan False $ \s c -> if s then Just False
                                     else if c == '"'
                                          then Nothing
                                          else Just (c == '\\')
    char '"'

    if '\\' `BS.elem` x
        then return $! String . decodeUtf8 . rep "\\\"" "\"". rep "\\\\" "\\" . rep "\\n" "\n" . rep "\\r" "\r" . rep "\\t" "\t" $ x
        else return $! String . decodeUtf8 $ x

    where rep f t s = BS.concat . BSL.toChunks $! replace (BS.pack f) (BS.pack t) s

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
    (ns, val) <- withNS c <|> withoutNS c
    return $! symbol ns val
    where
        withNS c = do
            ns <- takeWhile (inClass "a-zA-Z0-9#:.*!?+_-")
            char '/'
            val <- takeWhile (inClass "a-zA-Z0-9#:.*!?+_-")
            return (c `BS.cons` ns, val)

        withoutNS c = do
            val <- takeWhile (inClass "a-zA-Z0-9#:.*!?+_-")
            return ("", c `BS.cons` val)

parseKeyword :: Parser Value
parseKeyword = do
    skipSpace
    char ':'
    x <- takeWhile (inClass "a-zA-Z0-9.*/!?+_-")
    return $! Keyword x

parseNumber :: Parser Value
parseNumber = do
    skipSpace
    n <- number
    case n of
        I i -> return $! Integer i
        D d -> return $! Floating d

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

parseDiscard :: Parser ()
parseDiscard = do
    string "#_"
    parseValue
    return ()

parseValue :: Parser Value
parseValue = do
    skipSpace
    skipMany parseDiscard

    parseSet <|> parseMap
             <|> parseVector <|> parseList
             <|> parseNil <|> parseBool
             <|> parseNumber
             <|> parseKeyword <|> parseSymbol
             <|> parseCharacter
             <|> parseString

parseTagged :: Parser TaggedValue
parseTagged = do
    skipSpace
    withNS <|> withoutNS <|> noTag
    where
        withNS = do
            char '#'
            ns <- takeWhile (inClass "a-zA-Z0-9-")
            char '/'
            tag <- takeWhile (inClass "a-zA-Z0-9-")
            value <- parseValue
            return $! Tagged value ns tag

        withoutNS = do
            char '#'
            tag <- takeWhile (inClass "a-zA-Z0-9-")
            value <- parseValue
            return $! Tagged value "" tag

        noTag = do
            value <- parseValue
            return $! NoTag value
