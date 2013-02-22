{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Database.HDBC.SqlValue (toSql, fromSql, SqlValue)

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Data.Ratio
import Data.Time
import Data.Fixed
import Data.Convertible (Convertible(..))
import qualified System.Time as ST

import Debug.Trace(trace)

ts s = trace (show s) s

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b
             -- && zonedTimeZone a == zonedTimeZone b

    

commonChecks :: (Convertible a SqlValue, Convertible SqlValue a, Eq a) => a -> Bool
commonChecks a = (a == (fromSql $ toSql a))
                 && (a == (fromSql $ toSql (fromSql $ toSql a :: B.ByteString)))

clockTimeChecks :: ST.ClockTime -> Bool
clockTimeChecks c = dteq c (fromSql $ toSql c)
                    -- && dteq c (fromSql $ toSql (fromSql $ toSql c :: B.ByteString))
  where
    dteq a b = case ST.diffClockTimes a b of
      ST.TimeDiff { ST.tdYear = 0
                  , ST.tdMonth = 0
                  , ST.tdDay = 0
                  , ST.tdHour = 0
                  , ST.tdMin = 0
                  , ST.tdSec = 0 } -> True -- Pico Seconds may differ
      _ -> False

timeDiffChecks :: ST.TimeDiff -> Bool
timeDiffChecks t = tdeq nt (fromSql $ toSql nt)

  where
    nt = ST.normalizeTimeDiff t
    tdeq :: ST.TimeDiff -> ST.TimeDiff -> Bool
    tdeq a b = and [ zeq ST.tdYear
                   , zeq ST.tdMonth
                   , zeq ST.tdDay
                   , zeq ST.tdHour
                   , zeq ST.tdMin
                   , zeq ST.tdSec   
                   ]
      where
        zeq fn = (fn a) == (fn b)

genTimeDiff :: Gen ST.TimeDiff
genTimeDiff = ST.TimeDiff
              <$> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)
              <*> (getNonNegative <$> arbitrary)

zonedTimeCheck :: ZonedTime -> Bool
zonedTimeCheck t = diffUTCTime a b <= 1
  where
    a = zonedTimeToUTC t
    b = zonedTimeToUTC $ fromSql $ toSql t

timeOfDayTimeZoneChecks :: (TimeOfDay, TimeZone) -> Bool
timeOfDayTimeZoneChecks = undefined
  
sqlvalues :: Spec
sqlvalues = describe "SqlValue should be convertible" $ do
  prop "with string" $ \(s::String) -> commonChecks s
  prop "with text" $ \(t::T.Text) -> commonChecks t
  prop "with lazy text" $ \(t::TL.Text) -> commonChecks t
  prop "with bytestring" $ \(b::B.ByteString) -> commonChecks b
  prop "with lazy bytestring" $ \(b::BL.ByteString) -> commonChecks b
  prop "with int" $ \(i :: Int) -> commonChecks i
  prop "with int32" $ \(i :: Int32) -> commonChecks i
  prop "with int64" $ \(i :: Int64) -> commonChecks i 
  prop "with word32" $ \(w :: Word32) -> commonChecks w 
  prop "with word64" $ \(w :: Word64) -> commonChecks w 
  prop "with Integer" $ \(i :: Integer) -> commonChecks i 
  prop "with Bool" $ \(b :: Bool) -> commonChecks b 
  prop "with Char" $ \(c :: Char) -> commonChecks c
  prop "with Double" $ \(d :: Double) -> commonChecks d
  prop "with Rational" $ \(r :: Rational) -> commonChecks r
  prop "with Day" $ \(d :: Day) -> commonChecks d
  prop "with TimeOfDay" $ \(tod :: TimeOfDay) -> commonChecks tod
  prop "with (TimeOfDay, TimeZone)" timeOfDayTimeZoneChecks 
  prop "with LocalTime" $ \(lt :: LocalTime) -> commonChecks lt
  prop "with ZonedTime" $ zonedTimeCheck
  prop "with UTCTime" $ \(ut :: UTCTime) -> commonChecks ut
  prop "with Pico" $ \(p :: Pico) -> commonChecks p
  prop "with NormalDiffTime" $ \(nd :: NominalDiffTime) -> commonChecks nd
  prop "with ClockTime" $ \(ct :: ST.ClockTime) -> clockTimeChecks ct
  -- prop "with TimeDiff" $ forAll genTimeDiff timeDiffChecks -- FIXME: the problem with conversion is detected somewhere in convertible, I belive...
  prop "with DiffTime" $ \(td :: DiffTime) -> commonChecks td
  prop "with CalendarTime" $ \(ct :: ST.CalendarTime) -> commonChecks ct
  prop "with Maybe Int" $ \(mi :: Maybe Int) -> mi == (fromSql $ toSql mi) -- can not represent Null as ByteString

main = do
  hspec $ sqlvalues