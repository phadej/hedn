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
cases = map (\(i, o) -> (i, E.NoTag o)) noTags ++ haveTags

noTags :: [(BSL.ByteString, E.Value)]
noTags = [ ("nil", Nil)

         , ("true", E.Boolean True)
         , ("false", E.Boolean False)

         , ("\"a nice string\"", E.String "a nice string")
         , ("\"split\\second \\t\\rai\\n\"", E.String "split\\second \t\rai\n")
         , ("\"test \\\"sausage\\\" shmest\"", E.String "test \"sausage\" shmest")
         , ("\"\"", E.String "")

         , ("\\c", E.Character 'c')
         , ("\\\\", E.Character '\\')
         , ("\\newline", E.Character '\n')
         , ("\\space", E.Character ' ')
         , ("\\tab", E.Character '\t')

         , ("justasymbol", E.Symbol "" "justasymbol")
         , ("with#stuff:inside", E.Symbol "" "with#stuff:inside")
         , ("my-namespace/foo", E.Symbol "my-namespace" "foo")
         , ("/", E.Symbol "" "/")

         , (":fred", E.Keyword "fred")
         , (":my/fred", E.Keyword "my/fred")

         , ("42", E.Integer 42)
         , ("-1", E.Integer (-1))

         , ("100.50", E.Floating 100.5)
         , ("-3.14", E.Floating (-3.14))
         -- ...and many other strange stuff...

         , ("(a b 42)", sampleList)
         , ("()", E.List [])

         , ("[a b 42]", sampleVec)
         , ("[]", E.makeVec [])

         , ("{:a 1 \"foo\" :bar [1 2 3] four}", sampleMap)
         , ("{}", E.makeMap [])

         , ("#{a b [1 2 3]}", sampleSet)
         , ("#{}", E.makeSet [])

         , ("[a b #_foo 42]", sampleDiscard)
         , ("(1 2 ;more to go!\n 3 4)", sampleComment)
         ]

haveTags :: [(BSL.ByteString, E.TaggedValue)]
haveTags = [ ("#myapp/Person {:first \"Fred\" :last \"Mertz\"}", E.Tagged sampleTaggedMap "myapp" "Person")
           , ("{:first \"Fred\" :last \"Mertz\"}", E.NoTag sampleTaggedMap)
           ]

sampleList :: E.Value
sampleList = E.makeList [E.Symbol "" "a", E.Symbol "" "b", E.Integer 42]

sampleVec :: E.Value
sampleVec = E.makeVec [E.Symbol "" "a", E.Symbol "" "b", E.Integer 42]

sampleMap :: E.Value
sampleMap = E.makeMap [ (E.Keyword "a",                                     E.Integer 1)
                      , (E.String "foo",                                    E.Keyword "bar")
                      , (E.makeVec [E.Integer 1, E.Integer 2, E.Integer 3], E.Symbol "" "four")
                      ]

sampleSet :: E.Value
sampleSet = E.makeSet [ E.Symbol "" "a"
                      , E.Symbol "" "b"
                      , E.makeVec [E.Integer 1, E.Integer 2, E.Integer 3]
                      ]

sampleDiscard :: E.Value
sampleDiscard = E.makeVec [E.Symbol "" "a", E.Symbol "" "b", E.Integer 42]

sampleComment :: E.Value
sampleComment = E.makeList [E.Integer 1, E.Integer 2, E.Integer 3, E.Integer 4]

sampleTaggedMap :: E.Value
sampleTaggedMap = E.makeMap [ (E.Keyword "first", E.String "Fred")
                            , (E.Keyword "last", E.String "Mertz")
                            ]
