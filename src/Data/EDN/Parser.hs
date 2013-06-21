{-# LANGUAGE OverloadedStrings #-}

-- | Parse an UTF-8 encoded EDN string into a haskell representation of EDN objects.
-- Use 'Data.EDN.decode' to get actual types.

module Data.EDN.Parser (
    -- * Data parsers
    parseMaybe, parseBSL, parseBS, parseT, parseTL, parseS,
    -- * Attoparsec implementation
    parseValue, parseTagged
) where

import           Control.Applicative        (pure, (*>), (<|>))
import           Data.Attoparsec.Char8      as A
import           Data.Attoparsec.Combinator ()
import qualified Data.Attoparsec.Lazy       as AL
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.ByteString.Search     (replace)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prelude                    hiding (String, takeWhile)

import           Data.EDN.Types             (Tagged (..), TaggedValue,
                                             Value (..), makeMap, makeSet,
                                             makeVec)

isSpaceOrComma :: Char -> Bool
isSpaceOrComma ' ' = True
isSpaceOrComma '\r' = True
isSpaceOrComma '\n' = True
isSpaceOrComma '\t' = True
isSpaceOrComma ',' = True
isSpaceOrComma _ = False

spaceOrComma :: Parser Char
spaceOrComma = satisfy isSpaceOrComma <?> "space/comma"

skipSoC :: Parser ()
skipSoC = skipWhile isSpaceOrComma

parseNil :: Parser Value
parseNil = do
    skipSoC
    string "nil"
    return Nil

parseBool :: Parser Value
parseBool = do
    skipSoC
    choice [ string "true" *> pure (Boolean True)
           , string "false" *> pure (Boolean False)
           ]

parseString :: Parser Value
parseString = do
    skipSoC
    char '"'

    x <- A.scan False $ \s c -> if s then Just False
                                     else if c == '"'
                                          then Nothing
                                          else Just (c == '\\')
    char '"'

    if '\\' `BS.elem` x
        then return $! String
                     . TE.decodeUtf8
                     . rep "\\\"" "\""
                     . rep "\\\\" "\\"
                     . rep "\\n" "\n"
                     . rep "\\r" "\r"
                     . rep "\\t" "\t"
                     $ x
        else return $! String . TE.decodeUtf8 $ x

    where rep f t s = BS.concat . BSL.toChunks $! replace (BS.pack f) (BS.pack t) s

parseCharacter :: Parser Value
parseCharacter = do
    skipSoC
    char '\\'
    x <- string "newline" <|> string "space" <|> string "tab" <|> A.take 1
    return . Character $! case x of
                              "newline" -> '\n'
                              "return" -> '\r'
                              "space" -> ' '
                              "tab" -> '\t'
                              _ -> BS.head x

parseSymbol :: Parser Value
parseSymbol = do
    skipSoC
    c <- satisfy (inClass "a-zA-Z.*/!?$%&=+_-")
    (ns, val) <- withNS c <|> withoutNS c
    return $! Symbol ns val
    where
        withNS c = do
            ns <- takeWhile (inClass "a-zA-Z0-9#:.*!?$%&=+_-")
            char '/'
            vc <- satisfy (inClass "a-zA-Z.*/!?$%&=+_-")
            val <- takeWhile1 (inClass "a-zA-Z0-9#:.*!?$%&=+_-")
            return (c `BS.cons` ns, vc `BS.cons` val)

        withoutNS c = do
            val <- takeWhile (inClass "a-zA-Z0-9#:.*!?$%&=+_-")
            return ("", c `BS.cons` val)

parseKeyword :: Parser Value
parseKeyword = do
    skipSoC
    char ':'
    c <- satisfy (inClass "a-zA-Z.*/!?$%&=+_-")
    x <- takeWhile (inClass "a-zA-Z0-9#:.*/!?$%&=+_-")
    return $! Keyword (c `BS.cons` x)

parseNumber :: Parser Value
parseNumber = do
    skipSoC
    n <- number
    case n of
        I i -> return $! Integer i
        D d -> return $! Floating d

parseList :: Parser Value
parseList = do
    skipSoC
    char '('
    A.skipSpace
    vs <- parseTagged `sepBy` spaceOrComma
    A.skipSpace
    char ')'
    return $! List vs

parseVector :: Parser Value
parseVector = do
    skipSoC
    char '['
    A.skipSpace
    vs <- parseTagged `sepBy` spaceOrComma
    A.skipSpace
    char ']'
    return $! makeVec vs

parseMap :: Parser Value
parseMap = do
    skipSoC
    char '{'
    A.skipSpace
    pairs <- parseAssoc `sepBy` spaceOrComma
    A.skipSpace
    char '}'
    return $! makeMap pairs
    where
        parseAssoc = do
            key <- parseValue
            val <- parseTagged
            return (key, val)

parseSet :: Parser Value
parseSet = do
    skipSoC
    char '#'
    char '{'
    A.skipSpace
    vs <- parseTagged `sepBy` spaceOrComma
    A.skipSpace
    char '}'
    return $! makeSet vs

skipComment :: Parser ()
skipComment = skipSoC >> char ';' >> skipWhile (/= '\n')

parseDiscard :: Parser ()
parseDiscard = do
    skipSoC
    string "#_"
    parseValue
    return ()

-- | Parse a \"raw\" EDN value into a 'Value'.
parseValue :: Parser Value
parseValue = do
    skipSoC
    skipMany skipComment
    skipMany parseDiscard

    parseSet <|> parseMap
             <|> parseVector <|> parseList
             <|> parseNil <|> parseBool
             <|> parseNumber
             <|> parseKeyword <|> parseSymbol
             <|> parseCharacter
             <|> parseString

-- | Parse a probably tagged EDN value into a 'TaggedValue'.
parseTagged :: Parser TaggedValue
parseTagged = do
    skipSoC
    withNS <|> withoutNS <|> noTag
    where
        withNS = do
            char '#'
            ns <- takeWhile1 (inClass "a-zA-Z0-9-")
            char '/'
            tag <- takeWhile1 (inClass "a-zA-Z0-9-")
            value <- parseValue
            return $! Tagged value ns tag

        withoutNS = do
            char '#'
            tag <- takeWhile1 (inClass "a-zA-Z0-9-")
            value <- parseValue
            return $! Tagged value "" tag

        noTag = do
            value <- parseValue
            return $! NoTag value

-- | Parse a lazy 'BSL.ByteString' into a 'TaggedValue'. If fails due to incomplete or invalid input, 'Nothing' is returned.
parseMaybe :: BSL.ByteString -> Maybe TaggedValue
parseMaybe src = case parseBSL src of
    AL.Done _ r -> Just r
    _           -> Nothing

-- | Parse a lazy 'BSL.ByteString'.
parseBSL :: BSL.ByteString -> AL.Result TaggedValue
parseBSL = AL.parse parseTagged
{-# INLINE parseBSL #-}

-- | Parse a strict 'BS.ByteString', but without continutations.
parseBS :: BS.ByteString -> AL.Result TaggedValue
parseBS s = parseBSL . BSL.fromChunks $ [s]
{-# INLINE parseBS #-}

-- | Parse a lazy 'TL.Text'.
parseTL :: TL.Text -> AL.Result TaggedValue
parseTL = parseBSL . TLE.encodeUtf8
{-# INLINE parseTL #-}

-- | Parse a strict 'T.Text'.
parseT :: T.Text -> AL.Result TaggedValue
parseT = parseBS . TE.encodeUtf8
{-# INLINE parseT #-}

-- | Parse a string AKA '[Char]'. Not really useful other than for debugging purposes.
parseS :: [Char] -> AL.Result TaggedValue
parseS = parseBSL . BSL.pack
{-# INLINE parseS #-}
