module Main where
import           Test.Hspec.HUnit         (fromHUnitTest)
import           Test.Hspec.Runner        (hspec)

import           Data.EDN.Test.QuickCheck (ednSerializationProperties)
import           Data.EDN.Test.Unit       (tests)

main :: IO ()
main = hspec $ do
  fromHUnitTest tests
  ednSerializationProperties
