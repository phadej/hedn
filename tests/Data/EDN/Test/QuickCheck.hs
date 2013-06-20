{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.EDN.Test.QuickCheck where

import           Data.EDN.Test.QuickCheck.TH
import           Data.Time                   (UTCTime)

import qualified Data.Time.Calendar          as Calendar
import           Data.Time.Clock             (UTCTime (..), UniversalTime (..),
                                              secondsToDiffTime)
import           Data.Time.LocalTime         (localTimeToUT1, localTimeToUTC,
                                              ut1ToLocalTime, utc,
                                              utcToLocalTime)

import qualified Test.QuickCheck             as QC

-- * UTCTime Arbitrary instance ------------------------------------------------

fromGregorian :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
fromGregorian year month day hours minutes seconds =
    UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
  where
    day'     = Calendar.fromGregorian year month day
    seconds' = 3600 * hours + 60 * minutes + seconds

fromUniversalTime :: UniversalTime -> UTCTime
fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0

toUniversalTime :: UTCTime -> UniversalTime
toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

fromGregorian' :: Integer -> Int -> Int -> UTCTime
fromGregorian' y m d = fromGregorian y m d 0 0 0

startOfTime :: UTCTime
startOfTime = fromGregorian' 1970 1 1

fromMJD :: Rational -> UTCTime
fromMJD = fromUniversalTime . ModJulianDate

fromMJD' :: Float -> UTCTime
fromMJD' = fromMJD . realToFrac

startOfTimeMJD :: Rational
startOfTimeMJD = toMJD startOfTime

toMJD :: UTCTime -> Rational
toMJD = getModJulianDate . toUniversalTime

instance QC.Arbitrary UTCTime where
    -- coarbitrary = undefined
    arbitrary = do
      offset <- QC.choose (0, 20000) :: QC.Gen Float
      return . fromMJD' $ offset + fromRational startOfTimeMJD

--------------------------------------------------------------------------------

type MUTCTime = Maybe UTCTime
type Tuple  = (Int, Int)
type Triple = (Int, String, UTCTime)
type Tuple4 = (Int, String, [Maybe UTCTime], [Maybe Bool])
type Tuple5 = (Int, [String], [Maybe UTCTime], [Maybe Bool], (Bool, Bool))

$(mkSerializeSpecs [
                   -- ''Char    -- encode/decode are not isomorphic with Char
                   --  ''Double -- encode/decode are not isomorphic with Double
                   -- ''String
                     ''UTCTime
                   , ''MUTCTime
                   -- , ''Tuple
                   -- , ''Triple
                   -- , ''Tuple4
                   -- , ''Tuple5
                   ])
