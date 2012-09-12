{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.EDN.Parser as P
import Data.EDN.Types as E

import Control.Monad (mapM_)
import System.Exit (exitFailure, exitSuccess)
import qualified System.Console.ANSI as ANSI

main = mapM_ check cases

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
        , ("\"split\\nsecond \\t\\rai\\n\"", E.String "split\nsecond \t\rai\n", "")
        , ("\"test \\\"sausage\\\" shmest\"", E.String "test \"sausage\" shmest", "")

        , ("\\c", E.Character 'c', "")
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

        , ("[a b 42]", sampleVec, "")

        , ("{:a 1 \"foo\" :bar [1 2 3] four}", sampleMap, "")

        , ("#{a b [1 2 3]}", sampleSet, "")
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
