{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.EDN.Test.Unit (tests) where

import Test.HUnit

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M

import Data.EDN (encode, decode, ToEDN, toEDN, FromEDN, fromEDN)
import Data.EDN.Types as E
import Data.EDN.Types.Class (Result(..), (.=))
import Data.EDN.Parser (parseMaybe)

tests :: Test
tests = TestList [ TestLabel "BSL -> TV parser" $ TestList $ map makeParserCase parserCases
                 , TestLabel "TV -> BSL encoder" $ TestList $ map makeEncodeCase encodeCases
                 , TestLabel "ToEDN conversion" $ TestList $ map makeToEDNcase toEDNcases
                 , TestLabel "FromEDN converter/decoder" $ TestList $ map makeFromEDNcase fromEDNcases
                 , TestLabel "decoder fail" (TestCase (assertEqual "bad unicode" Nothing (parseMaybe "№")))
                 , TestLabel "'Tagged a' conversion" $ TestList taggedConversion
                 ]

makeParserCase :: (BSL.ByteString, E.TaggedValue) -> Test
makeParserCase (i, o) = TestCase (assertEqual (BSL.unpack i) (Just o) (parseMaybe i))

makeEncodeCase :: (E.TaggedValue, BSL.ByteString) -> Test
makeEncodeCase (i, o) = TestCase (assertEqual (BSL.unpack o) o (encode i))

parserCases :: [(BSL.ByteString, E.TaggedValue)]
parserCases = [ ("nil", E.nil)

              , ("true", E.true)
              , ("false", E.false)

              , ("\"a nice string\"", "a nice string")
              , ("\"split\\second \\t\\rai\\n\"", "split\\second \t\rai\n")
              , ("\"test \\\"sausage\\\" shmest\"", "test \"sausage\" shmest")
              , ("\"\"", "")

              , ("\\c", E.char 'c')
              , ("\\\\", E.char '\\')
              , ("\\newline", E.char '\n')
              , ("\\space", E.char ' ')
              , ("\\tab", E.char '\t')

              , ("justasymbol", E.symbol "justasymbol")
              , ("with#stuff:inside", E.symbol "with#stuff:inside")
              , ("my-namespace/foo", E.symbolNS "my-namespace" "foo")
              , ("/", E.symbol "/")

              , (":fred", E.keyword "fred")
              , (":my/fred", E.keyword "my/fred")

              , ("42", E.integer 42)
              , ("-1", E.integer (-1))

              , ("100.50", E.floating 100.5)
              , ("-3.14", E.floating (-3.14))
               -- ...and many other strange stuff...

              , ("(a b 42)", sampleList)
              , ("()", E.notag $ E.makeList [])

              , ("[a b 42]", sampleVec)
              , ("[]", E.notag $ E.makeVec [])

              , ("{:a 1, \"foo\" :bar, [1 2 3] four}", sampleMap)
              , ("{}", E.notag $ E.makeMap [])

              , ("#{a b [1 2 3]}", sampleSet)
              , ("#{}", E.notag $ E.makeSet [])

              , ("[a b #_foo 42]", sampleDiscard)
              , ("(1 2 ;more to go!\n 3 4)", sampleComment)

              , ("#myapp/Person {:first \"Fred\" :last \"Mertz\"}", E.tag "myapp" "Person" sampleTaggedMap)
              , ("{:first \"Fred\" :last \"Mertz\"}", E.notag sampleTaggedMap)
              ]

encodeCases :: [(E.TaggedValue, BSL.ByteString)]
encodeCases = [ (E.nil, "nil")

              , (E.true, "true")
              , (E.false, "false")

              , (E.string "a nice string", "\"a nice string\"")
              , ("overloaded", "\"overloaded\"")
              , ("escape \rou\te \\ \"\north\"", "\"escape \\rou\\te \\\\ \\\"\\north\\\"\"")

              , (E.char 'c', "\\c")
              , (E.char '\\', "\\\\")
              , (E.char '\n', "\\newline")
              , (E.char '\r', "\\return")
              , (E.char '\t', "\\tab")
              , (E.char ' ', "\\space")

              , (E.symbol "justasymbol", "justasymbol")
              , (E.symbol "with#stuff:inside", "with#stuff:inside")
              , (E.symbolNS "whatever" "whenever", "whatever/whenever")
              , (E.symbol "/", "/")

              , (E.keyword "fred", ":fred")
              , (E.keyword "my/fred", ":my/fred")
              , (E.notag ":overloaded", ":overloaded") -- IsString kw transform only for untagged values

              , (E.integer 42, "42")
              , (E.integer (-1), "-1")

              , (E.floating 100.50, "100.5")
              , (E.floating (-3.14), "-3.14")

              , (sampleList, "(a b 42)")
              , (E.notag $ E.makeList [], "()")

              , (sampleVec, "[a b 42]")
              , (E.notag $ E.makeVec [], "[]")

              , (sampleMap, "{\"foo\" :bar :a 1 [1 2 3] four}") -- Order not guaranteed
              , (E.notag $ E.makeMap [], "{}")

              , (E.tag "myapp" "Person" sampleTaggedMap, "#myapp/Person {:first \"Fred\" :last \"Mertz\"}")
              ]

sampleList :: E.TaggedValue
sampleList = E.notag $ E.makeList [E.symbol "a", E.symbol "b", E.integer 42]

sampleVec :: E.TaggedValue
sampleVec = E.notag $ E.makeVec [E.symbol "a", E.symbol "b", E.integer 42]

sampleMap :: E.TaggedValue
sampleMap = E.notag $ E.makeMap [ (":a",                                              E.integer 1)
                                , ("foo",                                             E.keyword "bar")
                                , (E.makeVec [E.integer 1, E.integer 2, E.integer 3], E.symbol "four")
                                ]

sampleSet :: E.TaggedValue
sampleSet = E.notag $ E.makeSet [ E.symbol "a"
                                , E.symbol "b"
                                , E.notag $ E.makeVec [E.integer 1, E.integer 2, E.integer 3]
                                ]

sampleDiscard :: E.TaggedValue
sampleDiscard = E.notag $ E.makeVec [E.symbol "a", E.symbol "b", E.integer 42]

sampleComment :: E.TaggedValue
sampleComment = E.notag $ E.makeList [E.integer 1, E.integer 2, E.integer 3, E.integer 4]

sampleTaggedMap :: E.Value
sampleTaggedMap = E.makeMap [ "first" .= E.string "Fred", "last" .= E.string "Mertz" ]

data ToEDNCase = forall a. ToEDN a => ToEDNCase !a !E.TaggedValue

makeToEDNcase :: ToEDNCase -> Test
makeToEDNcase (ToEDNCase i o) = TestCase (assertEqual (show o) o (toEDN i))

toEDNcases :: [ToEDNCase]
toEDNcases = [ ToEDNCase (Nothing :: Maybe Bool) E.nil

             , ToEDNCase (Left "hi" :: Either String ()) $ E.tag "either" "left" "hi"
             , ToEDNCase (Right "z" :: Either () String) $ E.tag "either" "right" "z"
             , ToEDNCase (Just '!') (E.char '!')

             , ToEDNCase True E.true
             , ToEDNCase False E.false

             , ToEDNCase ("test" :: String) (E.string "test")
             , ToEDNCase ("test" :: BSL.ByteString) "test"
             , ToEDNCase ("test" :: BS.ByteString) "test"
             , ToEDNCase ("test" :: TL.Text) "test"
             , ToEDNCase ("test" :: T.Text) "test"

             , ToEDNCase 'c' $ E.char 'c'
             , ToEDNCase '\n' $ E.char '\n'
             , ToEDNCase '\\' $ E.char '\\'

             , ToEDNCase (3.14 :: Double) $ E.floating 3.14
             , ToEDNCase (42 :: Integer) $ E.integer 42

             , ToEDNCase () (E.notag $ E.makeList [])
             , ToEDNCase ([] :: [()]) (E.notag $ E.makeList [])
             , ToEDNCase (["yo", "wassup"] :: [String]) (E.notag $ E.makeList ["yo", "wassup"])
             , ToEDNCase [E.string "i", "am", E.symbolNS "boxed" "container"] (E.notag $ E.makeList [E.string "i", "am", E.symbolNS "boxed" "container"])

             , ToEDNCase (V.fromList ['o', 'm', 'g']) (E.notag $ E.makeVec [E.char 'o', E.char 'm', E.char 'g'])

             , ToEDNCase (S.fromList ['o', 'm', 'g']) (E.notag $ E.makeSet [E.char 'o', E.char 'm', E.char 'g'])

             , ToEDNCase (M.fromList [("test", "shmest"), ("foo", "bar")] :: M.Map String String) (E.notag $ E.makeMap [("test", "shmest"), ("foo", "bar")])
             , ToEDNCase (M.fromList [(":test", "shmest"), (":foo", "bar")] :: EDNMap) (E.notag $ E.makeMap ["test" .= E.string "shmest", "foo" .= E.string "bar"])
             ]

data FromEDNCase = forall a. (FromEDN a, Show a, Eq a) => FromEDNCase !BSL.ByteString !a

makeFromEDNcase :: FromEDNCase -> Test
makeFromEDNcase (FromEDNCase i o) = TestCase $ assertEqual (BSL.unpack i) (Just o) (decode i)

fromEDNcases :: [FromEDNCase]
fromEDNcases = [ FromEDNCase "nil" (Nothing :: Maybe Bool)

               , FromEDNCase "false" False
               , FromEDNCase "true" True

               , FromEDNCase "#either/left hi" (Left "hi" :: Either String ())
               , FromEDNCase "#either/right z" (Right "z" :: Either () String)
               , FromEDNCase "\"ok fine\"" (Just "ok fine" :: Maybe T.Text)
               , FromEDNCase "nil" (Nothing :: Maybe BSL.ByteString)

               , FromEDNCase "\\newline" '\n'
               , FromEDNCase "\\z" 'z'

               , FromEDNCase "42" (42 :: Integer)
               , FromEDNCase "-3.14" (-3.14 :: Double)

               , FromEDNCase "([1 2] [3 4])" ([(1,2), (3,4)]:: [(Int,Int)])
               , FromEDNCase "((1 2) (3 4))" ([[1,2], [3,4]]:: [[Int]])
               , FromEDNCase "[[1 2] [3 4]]" ([(1,2), (3,4)]:: [(Int, Int)])
               , FromEDNCase "[[1 2] [3 4]]" ([[1,2], [3,4]]:: [[Int]])
               , FromEDNCase "(hello world)" (["hello", "world"] :: [String])

               , FromEDNCase "[3 2 1 \"Kaboom!\"]" $ E.makeVec [E.integer 3, E.integer 2, E.integer 1, "Kaboom!"]
               , FromEDNCase "#{a/bag :of \"bugs\"}" $ E.makeSet [E.symbolNS "a" "bag", E.keyword "of", E.string "bugs"]
               , FromEDNCase "{json: \"like\", but/not: \"really\"}" $ (M.fromList [("json:", "like"), ("but/not:", "really")] :: M.Map String String)
               , FromEDNCase "{ json: \"like\", but/not: \"really\" }" $ (M.fromList [("json:", "like"), ("but/not:", "really")] :: M.Map String String)
               ]

taggedConversion :: [Test]
taggedConversion = [ TestCase (assertEqual "toEDN tagged" (E.tag "wo" "ot" (E.Boolean False)) (toEDN $ E.tag "wo" "ot" False))
                   , TestCase (assertEqual "toEDN notag" E.false (toEDN $ E.notag False))
                   , TestCase (assertEqual "fromEDN tagged" (Success $ E.tag "wo" "ot" False) (fromEDN $ E.tag "wo" "ot" (E.Boolean False)))
                   , TestCase (assertEqual "fromEDN notag" (Success $ E.notag False) (fromEDN $ E.false))
                   ]
