{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Instances ()
import Database.HDBC.SqlValue (toSql, fromSql, SqlValue)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Data.Ratio
import Data.Time
import Data.Fixed
import Data.Convertible (Convertible)
import qualified System.Time as ST

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b &&
             zonedTimeZone a == zonedTimeZone b

commonChecks :: (Convertible a SqlValue, Convertible SqlValue a, Eq a) => a -> Bool
commonChecks a = (a == (fromSql $ toSql a))
               && (a == (fromSql $ toSql (fromSql $ toSql a :: B.ByteString)))
  
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
  prop "with (TimeOfDay, TimeZone)" $ \(tt :: (TimeOfDay, TimeZone)) -> commonChecks tt
  prop "with LocalTime" $ \(lt :: LocalTime) -> commonChecks lt
  prop "with ZonedTime" $ \(zt :: ZonedTime) -> commonChecks zt 
  prop "with UTCTime" $ \(ut :: UTCTime) -> commonChecks ut
  prop "with Pico" $ \(p :: Pico) -> commonChecks p
  prop "with NormalDiffTime" $ \(nd :: NominalDiffTime) -> commonChecks nd
  prop "with ClockTime" $ \(ct :: ST.ClockTime) -> commonChecks ct
  prop "with TimeDiff" $ \(td :: ST.TimeDiff) -> commonChecks td
  prop "with DiffTime" $ \(td :: DiffTime) -> commonChecks td
  prop "with CalendarTime" $ \(ct :: ST.CalendarTime) -> commonChecks ct
  prop "with Maybe Int" $ \(mi :: Maybe Int) -> mi == (fromSql $ toSql mi) -- can not represent Null as ByteString

main = do
  hspec $ sqlvalues