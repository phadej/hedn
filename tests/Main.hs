{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Control.Monad (when)
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.EDN.Types as E
import Data.EDN.Parser (decode)
import Data.EDN.Encode (encode)

main :: IO ()
main = do
    (Counts _ _ e f) <- runTestTT tests
    when (e > 0 || f > 0) $ exitFailure

tests :: Test
tests = TestList [ TestLabel "BSL -> TV decoder" $ TestList $ map makeDecodeCase decodeCases
                 , TestLabel "TV -> BSL encoder" $ TestList $ map makeEncodeCase encodeCases
                 , TestLabel "decoder fail" (TestCase (assertEqual "bad unicode" Nothing (decode "№")))
                 ]

makeDecodeCase :: (BSL.ByteString, E.TaggedValue) -> Test
makeDecodeCase (i, o) = TestCase (assertEqual (BSL.unpack i) (Just o) (decode i))

makeEncodeCase :: (E.TaggedValue, BSL.ByteString) -> Test
makeEncodeCase (i, o) = TestCase (assertEqual (BSL.unpack o) o (encode i))

decodeCases :: [(BSL.ByteString, E.TaggedValue)]
decodeCases = [ ("nil", E.nil)

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
sampleTaggedMap = E.makeMap [ "first" .= "Fred", "last" .= "Mertz" ]
