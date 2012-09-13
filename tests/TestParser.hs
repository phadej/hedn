{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Main where

import Data.Attoparsec.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.EDN.Parser as P
import Data.EDN.Types as E

import Control.Monad (forM_)
import qualified System.Console.ANSI as ANSI

main :: IO ()
main = forM_ cases $ checkCase (parse P.parseTagged) (parserResult)

parserResult :: Eq a => Result a -> a -> Bool
parserResult result output = case result of
    Done "" o -> o == output
    _         -> False

checkCase :: forall a a1. (Show a1, Show a) => (BSL.ByteString -> a1)
                                            -> (a1 -> a -> Bool)
                                            -> (BSL.ByteString, a)
                                            -> IO ()
checkCase runner checker (input, output) = do
    putStr "Checking: '"
    BSL.putStr input
    putStrLn "'"

    putStrLn $ "Should be: " ++ show output

    let result = runner input
    let correct = checker result output

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (if correct then ANSI.Green else ANSI.Red)]
    putStr "Result: "
    ANSI.setSGR [ANSI.Reset]
    print result
    putStrLn ""

cases :: [(BSL.ByteString, E.TaggedValue)]
cases = [ ("nil", E.nil)

        , ("true", E.true)
        , ("false", E.false)

        , ("\"a nice string\"", E.string "a nice string")
        , ("\"split\\second \\t\\rai\\n\"", E.string "split\\second \t\rai\n")
        , ("\"test \\\"sausage\\\" shmest\"", E.string "test \"sausage\" shmest")
        , ("\"\"", E.string "")

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
        , ("()", E.NoTag $ E.makeList [])

        , ("[a b 42]", sampleVec)
        , ("[]", E.NoTag $ E.makeVec [])

        , ("{:a 1 \"foo\" :bar [1 2 3] four}", sampleMap)
        , ("{}", E.NoTag $ E.makeMap [])

        , ("#{a b [1 2 3]}", sampleSet)
        , ("#{}", E.NoTag $ E.makeSet [])

        , ("[a b #_foo 42]", sampleDiscard)
        , ("(1 2 ;more to go!\n 3 4)", sampleComment)

        , ("#myapp/Person {:first \"Fred\" :last \"Mertz\"}", E.tag "myapp" "Person" sampleTaggedMap)
        , ("{:first \"Fred\" :last \"Mertz\"}", E.NoTag sampleTaggedMap)
        ]

sampleList :: E.TaggedValue
sampleList = E.NoTag $ E.makeList [E.symbol "a", E.symbol "b", E.integer 42]

sampleVec :: E.TaggedValue
sampleVec = E.NoTag $ E.makeVec [E.symbol "a", E.symbol "b", E.integer 42]

sampleMap :: E.TaggedValue
sampleMap = E.NoTag $ E.makeMap [ (E.Keyword "a",                                     E.integer 1)
                                , (E.String "foo",                                    E.keyword "bar")
                                , (E.makeVec [E.integer 1, E.integer 2, E.integer 3], E.symbol "four")
                                ]

sampleSet :: E.TaggedValue
sampleSet = E.NoTag $ E.makeSet [ E.symbol "a"
                                , E.symbol "b"
                                , E.NoTag $ E.makeVec [E.integer 1, E.integer 2, E.integer 3]
                                ]

sampleDiscard :: E.TaggedValue
sampleDiscard = E.NoTag $ E.makeVec [E.symbol "a", E.symbol "b", E.integer 42]

sampleComment :: E.TaggedValue
sampleComment = E.NoTag $ E.makeList [E.integer 1, E.integer 2, E.integer 3, E.integer 4]

sampleTaggedMap :: E.Value
sampleTaggedMap = E.makeMap [ (E.Keyword "first", E.string "Fred")
                            , (E.Keyword "last", E.string "Mertz")
                            ]
