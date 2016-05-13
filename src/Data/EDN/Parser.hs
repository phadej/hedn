{-# LANGUAGE OverloadedStrings #-}

-- | Parse an UTF-8 encoded EDN string into a haskell representation of EDN objects.
-- Use 'Data.EDN.decode' to get actual types.

module Data.EDN.Parser (
    -- * Data parsers
    parseMaybe, parseEither, parseBSL, parseBS, parseT, parseTL, parseS,
    -- * Attoparsec implementation
    parseValue, parseTagged
) where

import Prelude ()
import Prelude.Compat hiding (String, takeWhile)

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.Combinator       ()
import qualified Data.Attoparsec.Lazy             as AL
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BSL
import           Data.ByteString.Search           (replace)
import qualified Data.ByteString.UTF8             as UTF8
import           Data.Maybe                       (fromJust)
import           Data.Scientific                  as Sci
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TLE

import           Data.EDN.Types                   (Tagged (..), TaggedValue,
                                                   Value (..), makeMap, makeSet,
                                                   makeVec)
import qualified Prelude                          as P

isSpaceOrComma :: Char -> Bool
isSpaceOrComma ' '  = True
isSpaceOrComma '\r' = True
isSpaceOrComma '\n' = True
isSpaceOrComma '\t' = True
isSpaceOrComma ','  = True
isSpaceOrComma _    = False

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
    choice [ string "true"  *> pure (Boolean True)
           , string "false" *> pure (Boolean False)
           ]

parseString :: Parser Value
parseString = do
    skipSoC
    char '"'

    x <- A.scan False $ \s c -> if s
                                then Just False
                                else if c == '"'
                                     then Nothing
                                     else Just (c == '\\')
    char '"'

    let prepare = if '\\' `BS.elem` x
                  then rep "\\\"" "\""
                       . rep "\\\\" "\\"
                       . rep "\\n" "\n"
                       . rep "\\r" "\r"
                       . rep "\\t" "\t"
                  else id

    return $! String $ TE.decodeUtf8 $ prepare x

    where
        rep f t s = BS.concat . BSL.toChunks
                    $! replace (BS.pack f) (BS.pack t) s

parseCharacter :: Parser Value
parseCharacter = do
    skipSoC
    char '\\'
    simple <|> anyCharUtf8

    where
        simple :: Parser Value
        simple = do
            x <- string "newline"
             <|> string "return"
             <|> string "space"
             <|> string "tab"
             <|> string "\\"

            return . Character $! case x of
              "newline" -> '\n'
              "return" -> '\r'
              "space" -> ' '
              "tab" -> '\t'
              "\\" -> '\\'
              _ -> error ("EDN.parseCharacter: impossible - simple" ++ show x)

        anyCharUtf8 :: Parser Value
        anyCharUtf8 = do
            bs <- scan BS.empty go
            case UTF8.decode bs of
                Just (c, _) -> return $! Character c
                Nothing     -> error $ "EDN.parseCharacter: bad utf8 data? " ++ show bs

        go :: BS.ByteString -> Char -> Maybe BS.ByteString
        go s c
            | BS.null s = Just (BS.singleton c)
            | otherwise = case UTF8.decode s of
                              Nothing      -> Just (s `BS.snoc` c)
                              Just (uc, _) -> if uc == UTF8.replacement_char
                                                  then Just (s `BS.snoc` c)
                                                  else Nothing

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
    c <- satisfy   (inClass "a-zA-Z.*/!?$%&=+_-")
    x <- takeWhile (inClass "a-zA-Z0-9#:.*/!?$%&=+_-")
    return $! Keyword (c `BS.cons` x)

parseNumber :: Parser Value
parseNumber = do
    skipSoC
    n <- A.scientific
    return $!
        if Sci.isInteger n
        then Integer  (fromIntegral (fromJust (Sci.toBoundedInteger n) :: P.Int))
        else Floating (Sci.toRealFloat n)


parseColl :: Parser t1       -- opening bracket
          -> Parser t2       -- closing bracket
          -> Parser a        -- item parser
          -> ([a] -> Value)  -- Value constructor
          -> Parser Value
parseColl openingBr closingBr item construct = do
    skipSoC
    _ <- openingBr
    A.skipSpace
    vs <- item `sepBy` spaceOrComma
    A.skipSpace
    _<- closingBr
    return $! construct vs

parseList :: Parser Value
parseList =
    parseColl (char '(') (char ')') parseTagged List

parseVector :: Parser Value
parseVector =
    parseColl (char '[') (char ']') parseTagged makeVec

parseSet :: Parser Value
parseSet =
    parseColl (char '#' *> char '{') (char '}') parseTagged makeSet

parseMap :: Parser Value
parseMap =
    parseColl (char '{') (char '}') parseAssoc makeMap
    where
        parseAssoc = do
            key <- parseValue
            val <- parseTagged
            return (key, val)


skipComment :: Parser ()
skipComment = skipSoC *> char ';' *> skipWhile (/= '\n')

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
            ns <- parseIdent
            char '/'
            tag <- parseIdent
            value <- parseValue
            return $! Tagged value ns tag

        withoutNS = do
            char '#'
            tag <- parseIdent
            value <- parseValue
            return $! Tagged value "" tag

        parseIdent = takeWhile1 (inClass "a-zA-Z0-9-")

        noTag = do
            value <- parseValue
            return $! NoTag value

{- | Parse a lazy 'BSL.ByteString' into a 'TaggedValue'.
If fails due to incomplete or invalid input, 'Nothing' is returned.
-}
parseMaybe :: BSL.ByteString -> Maybe TaggedValue
parseMaybe = AL.maybeResult . parseBSL

{- | Parse a lazy 'BSL.ByteString' into a 'TaggedValue'.
If fails due to incomplete or invalid input,
'Left' is returned with the error message.
-}
parseEither :: BSL.ByteString -> Either P.String TaggedValue
parseEither = AL.eitherResult . parseBSL

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
parseS :: P.String -> AL.Result TaggedValue
parseS = parseBSL . BSL.pack
{-# INLINE parseS #-}
