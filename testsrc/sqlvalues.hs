{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , CPP #-}

module SqlValues where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Property
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import Database.HDBC (ToSql(..), FromSql(..), BitField(..))

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Data.Decimal
import Data.Time
import Data.UUID


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


mainTestGroup :: Test
mainTestGroup = testGroup "can convert to SqlValue and back"
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

main :: IO ()
main = defaultMain [mainTestGroup]
