{-# LANGUAGE
  ScopedTypeVariables
, FlexibleContexts
, CPP
  #-}

module SqlValues where

import Control.Applicative
import Database.HDBC (ToSql(..), FromSql(..), BitField(..))
import Database.HDBC.Parsers
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property

import qualified Data.Attoparsec.Text.Lazy as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Decimal
import Data.Int
import Data.List (intercalate)
import Data.Time
import Data.UUID
import Data.Word


#if MIN_VERSION_Decimal(0,3,1)
-- Decimal-0.2.4 has no Arbitrary instance in library any more
instance (Arbitrary i, Integral i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
#endif

instance Arbitrary UUID where
  arbitrary = fromWords
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary BitField where
  arbitrary = BitField <$> arbitrary

instance Eq ZonedTime where
  (ZonedTime a b) == (ZonedTime aa bb) = a == aa && b == bb

-- commonChecks :: (Convertible a SqlValue, Convertible SqlValue a, Eq a, Show a) => a -> Property
commonChecks :: (Eq a, Show a, ToSql a, FromSql a) => a -> Property
commonChecks x = (partialChecks x) .&&.
                 (x ==? (fromSql $ toSql (fromSql $ toSql x :: TL.Text))) -- convert to Text and back

-- partialChecks :: (Eq a, Show a, Convertible a SqlValue, Convertible SqlValue a) => a -> Result
partialChecks :: (Eq a, Show a, ToSql a, FromSql a) => a -> Result
partialChecks x = x ==? (fromSql $ toSql x)

doubleChecks :: Double -> Property
doubleChecks x = (partialChecks x) .&&.
                 (x ~==? (fromSql $ toSql (fromSql $ toSql x :: TL.Text))) -- convert to Text and back


sqlValueTestGroup :: Test
sqlValueTestGroup = testGroup "can convert to SqlValue and back"
                [ testProperty "with string" $ \(s::String) -> commonChecks s
                , testProperty "with text" $ \(t::T.Text) -> commonChecks t
                , testProperty "with lazy text" $ \(t::TL.Text) -> commonChecks t
                , testProperty "with bytestring" $ \(b::B.ByteString) -> partialChecks b       -- not any bytestring can be converted to Text
                , testProperty "with lazy bytestring" $ \(b::BL.ByteString) -> partialChecks b -- same as above
                , testProperty "with int" $ \(i :: Int) -> commonChecks i
                , testProperty "with int32" $ \(i :: Int32) -> commonChecks i
                , testProperty "with int64" $ \(i :: Int64) -> commonChecks i
                , testProperty "with word" $ \(w :: Word) -> commonChecks w
                , testProperty "with word32" $ \(w :: Word32) -> commonChecks w
                , testProperty "with word64" $ \(w :: Word64) -> commonChecks w
                , testProperty "with Integer" $ \(i :: Integer) -> commonChecks i
                , testProperty "with Bool" $ \(b :: Bool) -> commonChecks b
                , testProperty "with BitField" $ \(bf :: BitField) -> commonChecks bf
                , testProperty "with Double" doubleChecks
                , testProperty "with Decimal" $ \(d :: Decimal) -> commonChecks d
                , testProperty "with Day" $ \(d :: Day) -> commonChecks d
                , testProperty "with UUID" $ \(u :: UUID) -> commonChecks u
                , testProperty "with TimeOfDay" $ \(tod :: TimeOfDay) -> commonChecks tod
                , testProperty "with LocalTime" $ \(lt :: LocalTime) -> commonChecks lt
                , testProperty "with UTCTime" $ \(ut :: UTCTime) -> commonChecks ut
                , testProperty "with Maybe Int" $ \(mi :: Maybe Int) -> mi == (fromSql $ toSql mi) -- can not represent Null as ByteString
                ]

parserFail :: [String] -> String -> String
parserFail cont msg = "parser failed in context: "
                      ++ (intercalate ", " cont)
                      ++ " with message: "
                      ++ msg

parsedTo :: (Eq a, Show a) => P.Parser a -> TL.Text -> a -> Assertion
parsedTo pr t res = case P.parse pr t of
  P.Fail _ cnt msg -> assertFailure $ parserFail cnt msg
  P.Done _ r       -> r @?= res

parseCase :: (Eq a, Show a) => String -> P.Parser a -> a -> Test
parseCase s p val = testCase s $ parsedTo p (TL.pack s) val

parserTests :: Test
parserTests = testGroup "can parse this dates and times"
              [ parseCase "1920-10-10" parseIsoDay $ fromGregorian 1920 10 10
              , parseCase "12:00:23" parseIsoTimeOfDay $ TimeOfDay 12 00 23
              , parseCase "2010 -3-25 22:11:00" parseIsoLocalTime
                $ LocalTime (fromGregorian 2010 3 25)
                $ TimeOfDay 22 11 0
              , parseCase "2013-07-01T00:00:00" parseIsoZonedTime
                $ ZonedTime
                (LocalTime (fromGregorian 2013 7 1)
                 $ TimeOfDay 0 0 0)
                utc
              , parseCase "2014-4-18 12:12:12+0400" parseIsoZonedTime
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 12)
                $ minutesToTimeZone $ 4 * 60
              , parseCase "2014-4-18 12:12:12+04" parseIsoZonedTime
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 12)
                $ minutesToTimeZone $ 4 * 60
              , parseCase "2014-4-18 12:12:12+04:00" parseIsoZonedTime
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 12)
                $ minutesToTimeZone $ 4 * 60
              , parseCase "2014-4-18 12:12:12+04:00:30" parseIsoZonedTime -- postgre's strange format
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 42)
                $ minutesToTimeZone $ 4 * 60
              , parseCase "2014-4-18 12:12:12.234-4" parseIsoZonedTime
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 12.234)
                $ minutesToTimeZone $ (-4) * 60
              , parseCase "2014-4-18 12:12:12.44-04:30:12" parseIsoZonedTime -- postgre's strange format
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 0.44)
                $ minutesToTimeZone $ (-4) * 60 - 30
              , parseCase "2014-4-18 12:12:12.44-043012" parseIsoZonedTime -- postgre's strange format
                $ ZonedTime
                (LocalTime (fromGregorian 2014 4 18)
                 $ TimeOfDay 12 12 0.44)
                $ minutesToTimeZone $ (-4) * 60 - 30
              ]

main :: IO ()
main = defaultMain [ sqlValueTestGroup
                   , parserTests
                   ]
