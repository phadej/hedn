{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.EDN.Parser as P
import Data.EDN.Types as E

import Control.Monad (mapM_)
import System.Exit (exitFailure, exitSuccess)
import qualified System.Console.ANSI as ANSI

main = do
    mapM_ check cases
    mapM_ checkTagged casesTagged

checkTagged (input, output, rest) = do
    putStr "Checking: '"
    BSL.putStr input
    putStrLn "'"

    putStr $ "Should be: " ++ show output
    if BSL.null rest
        then putStrLn ""
        else putStrLn $ " (" ++ BSL.unpack rest ++ ")"

    let result = parse P.parseTagged input
    let correct = case result of
                      Done r o -> r == rest && o == output
                      _ -> False

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (if correct then ANSI.Green else ANSI.Red)]
    putStr "Result: "
    ANSI.setSGR [ANSI.Reset]
    print result
    putStrLn ""

check (input, output, rest) = do
    putStr "Checking: '"
    BSL.putStr input
    putStrLn "'"

    putStr $ "Should be: " ++ show output
    if BSL.null rest
        then putStrLn ""
        else putStrLn $ " (" ++ BSL.unpack rest ++ ")"

    let result = parse P.parseValue input
    let correct = case result of
                      Done r o -> r == rest && o == output
                      _ -> False

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (if correct then ANSI.Green else ANSI.Red)]
    putStr "Result: "
    ANSI.setSGR [ANSI.Reset]
    print result
    putStrLn ""

cases = [ ("nil", Nil, "")
        , ("nil whatever", Nil, " whatever")

        , ("true", E.Boolean True, "")
        , ("false", E.Boolean False, "")

        , ("\"a nice string\"", E.String "a nice string", "")
        , ("\"split\\second \\t\\rai\\n\"", E.String "split\\second \t\rai\n", "")
        , ("\"test \\\"sausage\\\" shmest\"", E.String "test \"sausage\" shmest", "")
        , ("\"\"", E.String "", "")

        , ("\\c", E.Character 'c', "")
        , ("\\\\", E.Character '\\', "")
        , ("\\newline", E.Character '\n', "")
        , ("\\space", E.Character ' ', "")
        , ("\\tab", E.Character '\t', "")

        , ("justasymbol", E.symbol "" "justasymbol", "")
        , ("with#stuff:inside", E.symbol "" "with#stuff:inside", "")
        , ("my-namespace/foo", E.symbol "my-namespace" "foo", "")
        , ("/", E.symbol "" "/", "")

        , (":fred", E.Keyword "fred", "")
        , (":my/fred", E.Keyword "my/fred", "")

        , ("42", E.Integer 42, "")
        , ("-1", E.Integer (-1), "")

        , ("100.50", E.Floating 100.5, "")
        , ("-3.14", E.Floating (-3.14), "")
        -- ...and many other strange stuff...

        , ("(a b 42)", sampleList, "")
        , ("()", E.List [], "")

        , ("[a b 42]", sampleVec, "")
        , ("[]", E.makeVec [], "")

        , ("{:a 1 \"foo\" :bar [1 2 3] four}", sampleMap, "")
        , ("{}", E.makeMap [], "")

        , ("#{a b [1 2 3]}", sampleSet, "")
        , ("#{}", E.makeSet [], "")

        , ("[a b #_foo 42]", sampleDiscard, "")
        , ("(1 2 ;more to go!\n 3 4)", sampleComment, "")
        ]

casesTagged = [ ("#myapp/Person {:first \"Fred\" :last \"Mertz\"}", E.Tagged sampleTaggedMap "myapp" "Person", "")
              , ("#{a b [1 2 3]}", E.NoTag sampleSet, "")
              ]

sampleList = E.List [E.symbol "" "a", E.symbol "" "b", E.Integer 42]

sampleVec = E.makeVec [E.symbol "" "a", E.symbol "" "b", E.Integer 42]

sampleMap = E.makeMap [ (E.Keyword "a",                                     E.Integer 1)
                      , (E.String "foo",                                    E.Keyword "bar")
                      , (E.makeVec [E.Integer 1, E.Integer 2, E.Integer 3], E.symbol "" "four")
                      ]

sampleSet = E.makeSet [ E.symbol "" "a"
                      , E.symbol "" "b"
                      , E.makeVec [E.Integer 1, E.Integer 2, E.Integer 3]
                      ]

sampleDiscard = E.makeVec [E.symbol "" "a", E.symbol "" "b", E.Integer 42]
sampleComment = E.List [E.Integer 1, E.Integer 2, E.Integer 3, E.Integer 4]

sampleTaggedMap = E.makeMap [ (E.Keyword "first", E.String "Fred")
                            , (E.Keyword "last", E.String "Mertz")
                            ]
