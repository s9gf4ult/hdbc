{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  #-}

#if ! (MIN_VERSION_time(1,1,3))
{-# LANGUAGE
    StandaloneDeriving #-}
#endif

{- |
   Module     : Database.HDBI.SqlValue
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : BSD3

   Maintainer : Aleksey Uymanov <s9gf4ult@gmail.com>
   Stability  : experimental
   Portability: portable
-}


module Database.HDBI.SqlValue
    (
      ToSql(..)
    , FromSql(..)
    , ToRow(..)
    , FromRow(..)
    , ConvertError(..)
    , BitField(..)
      -- * SQL value marshalling
    , SqlValue(..)
    )

where

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Data.Attoparsec.Text.Lazy
import Data.Bits (Bits)
import Data.Data (Data)
import Data.Decimal
import Data.Int
import Data.Ix (Ix)
import Data.List (intercalate)
import Data.Time
import Data.Typeable
import Data.UUID (UUID, fromString, toString)
import Data.Word
import Database.HDBI.Formaters
import Database.HDBI.Parsers
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Convertion error description. Used in 'FromSql' typeclass.
data ConvertError =
  ConvertError { ceReason :: String -- ^ Detailed description of convertion error
               }
  -- | Type names must unique. Expecting names are generated by ('show' . 'typeOf')
  -- function
  | IncompatibleTypes { ceFromType :: String -- ^ name of type trying to convert
                                            -- from.
                      , ceToType   :: String -- ^ name of target type.
                      }
                  deriving (Show, Typeable, Eq)

instance Exception ConvertError


-- | Auxiliary type to represent bit field outside of SqlValue
newtype BitField = BitField { unBitField :: Word64 }
                   deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Real, Ix, Typeable, Bits)

instance Show BitField where
  show = formatBitField . unBitField

-- | All types must convert to SqlValue safely and unambiguously. That's why
-- there is no ''safeToSql'' method
class ToSql a where
  toSql :: a -> SqlValue

class FromSql a where
  safeFromSql :: SqlValue -> Either ConvertError a

  -- | Unsafe method, throws 'ConvertError' if convertion failed. Has default
  -- implementation.
  fromSql :: SqlValue -> a
  fromSql s = case safeFromSql s of
    Left e -> throw e
    Right a -> a

class ToRow a where
  toRow :: a -> [SqlValue]

class FromRow a where
  safeFromRow :: [SqlValue] -> Either ConvertError a

  fromRow :: [SqlValue] -> a
  fromRow sqls = case safeFromRow sqls of
    Left e -> throw e
    Right a -> a

wrongSqlList :: [SqlValue] -- ^ given list of SqlValues
                -> Int -- ^ expected length of list
                -> Either ConvertError a
wrongSqlList x c = Left $ ConvertError
                   $ "Wrong count of SqlValues: " ++ (show $ length x)
                   ++ " but expected: " ++ (show c)

-- instance (ToSql a) => ToRow a where
--   toRow a = [toSql a]

-- instance (FromSql a) => FromRow a where
--   safeFromRow [a] = safeFromSql a
--   safeFromRow x = wrongSqlList x 1

instance (ToSql a) => ToRow [a] where
  toRow a = map toSql a

instance (FromSql a) => FromRow [a] where
  safeFromRow a = mapM safeFromSql a

instance (ToSql a, ToSql b) => ToRow (a, b) where
  toRow (a, b) = [toSql a, toSql b]

instance (FromSql a, FromSql b) => FromRow (a, b) where
  safeFromRow [a, b] = (,) <$> safeFromSql a <*> safeFromSql b
  safeFromRow x = wrongSqlList x 2

instance (ToSql a, ToSql b, ToSql c) => ToRow (a, b, c) where
  toRow (a, b, c) = [toSql a, toSql b, toSql c]

instance (FromSql a, FromSql b, FromSql c) => FromRow (a, b, c) where
  safeFromRow [a, b, c] = (,,) <$> safeFromSql a <*> safeFromSql b <*> safeFromSql c
  safeFromRow x = wrongSqlList x 3

instance (ToSql a, ToSql b, ToSql c, ToSql d) => ToRow (a, b, c, d) where
  toRow (a, b, c, d) = [toSql a, toSql b, toSql c, toSql d]

instance (FromSql a, FromSql b, FromSql c, FromSql d) => FromRow (a, b, c, d) where
  safeFromRow [a, b, c, d] = (,,,) <$> safeFromSql a <*> safeFromSql b <*> safeFromSql c <*> safeFromSql d
  safeFromRow x = wrongSqlList x 4

instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e) => ToRow (a, b, c, d, e) where
  toRow (a, b, c, d, e) = [toSql a, toSql b, toSql c, toSql d, toSql e]

instance (FromSql a, FromSql b, FromSql c, FromSql d, FromSql e) => FromRow (a, b, c, d, e) where
  safeFromRow [a, b, c, d, e] = (,,,,)
                                <$> safeFromSql a
                                <*> safeFromSql b
                                <*> safeFromSql c
                                <*> safeFromSql d
                                <*> safeFromSql e
  safeFromRow x = wrongSqlList x 5


instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f) => ToRow (a, b, c, d, e, f) where
  toRow (a, b, c, d, e, f) = [toSql a, toSql b, toSql c, toSql d, toSql e, toSql f]

instance (FromSql a, FromSql b, FromSql c, FromSql d, FromSql e, FromSql f) => FromRow (a, b, c, d, e, f) where
  safeFromRow [a, b, c, d, e, f] = (,,,,,)
                                   <$> safeFromSql a
                                   <*> safeFromSql b
                                   <*> safeFromSql c
                                   <*> safeFromSql d
                                   <*> safeFromSql e
                                   <*> safeFromSql f
  safeFromRow x = wrongSqlList x 6

instance (ToSql a, ToSql b, ToSql c, ToSql d, ToSql e, ToSql f, ToSql g) => ToRow (a, b, c, d, e, f, g) where
  toRow (a, b, c, d, e, f, g) = [toSql a, toSql b, toSql c, toSql d, toSql e, toSql f, toSql g]

instance (FromSql a, FromSql b, FromSql c, FromSql d, FromSql e, FromSql f, FromSql g) => FromRow (a, b, c, d, e, f, g) where
  safeFromRow [a, b, c, d, e, f, g] = (,,,,,,)
                                      <$> safeFromSql a
                                      <*> safeFromSql b
                                      <*> safeFromSql c
                                      <*> safeFromSql d
                                      <*> safeFromSql e
                                      <*> safeFromSql f
                                      <*> safeFromSql g
  safeFromRow x = wrongSqlList x 7
  
  
  
-- | Show parser detail error
showFail :: [String]  -- ^ List of contexts of parser
            -> String -- ^ Error message
            -> String
showFail cont msg = "Parser failed in context "
                    ++ (show $ intercalate ", " cont)
                    ++ " with message "
                    ++ (show msg)


incompatibleTypes :: (Typeable a, Typeable b) => a -> b -> Either ConvertError c
incompatibleTypes a b = Left $ IncompatibleTypes (show $ typeOf a) (show $ typeOf b)

-- | create converting from Null error message
nullConvertError :: (Typeable a) => a -> Either ConvertError b
nullConvertError a = Left $ ConvertError ("could not convert SqlNull to " ++ (show $ typeOf a))

convertToBounded :: forall b. (Integral b, Typeable b, Bounded b) => Integer -> Either ConvertError b
convertToBounded a = if a > bmax
                     then errorval
                     else if a < bmin
                          then errorval
                          else Right $ fromIntegral a
  where
    bmin = toInteger (minBound :: b)
    bmax = toInteger (maxBound :: b)
    errorval = Left $ ConvertError ("The value " ++ show a ++ " is out of bounds of " ++ (show $ typeOf (undefined :: b)))

tryParse :: TL.Text -> Parser a -> Either ConvertError a
tryParse t parser = case parse parser t of
    Fail _ cont desc -> Left $ ConvertError $ showFail cont desc
    Done _ res       -> Right res


{- | 'SqlValue' is the main type for expressing Haskell values to SQL databases.

/WHAT IS SQLVALUE/

SqlValue is an intermediate type to store/recevie data to/from the
database. Every database driver will do it's best to properly convert any
SqlValue to the database record's field, and properly convert the record's field
to SqlValue back.

The 'SqlValue' has predefined 'FromSql' and 'ToSql' instances for many Haskell's
types. Any Haskell's type can be converted to the 'SqlValue' with 'toSql'
function. There is no safeToSql function because 'toSql' never fails. Also, any
'SqlValue' type can be converted to almost any Haskell's type as well. Not any
'SqlValue' can be converted back to Haskell's type, so there is 'safeFromSql'
function to do that safely. There is also unsafe 'toSql' function of caurse.

You can sure, that @fromSql . toSql == id@

/SQLVALUE CONSTRUCTORS/

'SqlValue' constructors is the MINIMAL set of constructors, required to
represent the most wide range of native database types.

For example, there is FLOAT native database type and DOUBLE, but any DOUBLE can
carry any FLOAT value, so there is no need to create 'SqlValue' constructor to
represent FLOAT type, we can do it with Double. But there is DECIMAL database
type, representing arbitrary precision value which can be carried just by
'Decimal' Haskell's type, so we need a constructor for it.

There is no SqlRational any more, because there is no one database which have
native Rational type. This is the key idea: if database can not store this type
natively we will not create 'SqlValue' clause for it.

Each 'SqlValue' constructor is documented or self-explaining to understand what
it is needed for.

/'ToSql' and 'FromSql' INSTANCES/

The key idea is to do the most obvious conversion between types only if it is
not ambiguous. For example, the most obvious conversion of 'Double' to 'Int32'
is just truncate the 'Double', the most obvious conversion of String to
'UTCTime' is to try read the 'String' as date and time. But there is no obvious
way to convert 'Int32' to 'UTCTime', so if you will try to convert ('SqlInteger'
44) to date you will fail. User must handle this cases properly converting
values with right way. It is not very good idea to silently perform strange and
ambiguous convertions between absolutely different data types.

/ERROR CONDITIONS/

There may be sometimes an error during conversion.  For instance, if you have an
'SqlText' and attempting to convert it to an 'Integer', but it doesn't parse as
an 'Integer', you will get an error.  This will be indicated as an exception
using 'fromSql', or a Left result using 'safeFromSql'.


/STORING SQLVALUE TO DATABASE/

Any 'SqlValue' can be converted to 'Text' and then readed from 'Text' back. This
is guaranteed by tests, so the database driver's author can use it to store and
read data through 'Text' for types which is not supported by the database
natively.

/TEXT AND BYTESTRINGS/

We are using lazy Text everywhere because it is faster than 'String' and has
builders. Strict text can be converted to one-chanked lazy text with O(1)
complexity, but lazy to strict converts with O(n) complexity, so it is logical
to use lazy Text.

We are not using ByteString as text encoded in UTF-8, ByteStrings are just
sequences of bytes. We are using strict ByteStrings because HDBI drivers uses
them to pass the ByteString to the C library as 'CString', so it must be strict.

We are not using 'String' as data of query or as query itself because it is not
effective in memory and cpu.

/DATE AND TIME/

We are not using time with timezone, because there is no one database working
with it natively except PostgreSQL, but the documentations of PostgreSQL says

/To address these difficulties, we recommend using date/time types that contain
both date and time when using time zones. We do not recommend using the type
time with time zone (though it is supported by PostgreSQL for legacy
applications and for compliance with the SQL standard). PostgreSQL assumes your
local time zone for any type containing only date or time./

This is not recomended to use time with timezone.

We are using 'UTCTime' instead of 'TimeWithTimezone' because no one database
actually save timezone information. All databases just convert datetime to
'UTCTime' when save data and convert UTCTime back to LOCAL SERVER TIMEZONE when
returning the data. So it is logical to work with timezones on the haskell side.

Time intervals are not widely supported, actually just in PostgreSQL and
Oracle. So, if you need them you can serialize throgh 'SqlText' by hands, or
write your own 'ToSql' and 'FromSql' instances to do that more convenient.

/EQUALITY OF SQLVALUE/

Two SqlValues are considered to be equal if one of these hold.  The
first comparison that can be made is controlling; if none of these
comparisons can be made, then they are not equal:

 * Both are NULL

 * Both represent the same type and the encapsulated values are considered equal
   by applying (==) to them

 * The values of each, when converted to a 'String', are equal.

-}
data SqlValue =
  -- | Arbitrary precision DECIMAL value
  SqlDecimal Decimal
  -- | Any Integer, including Int32, Int64 and Words.
  | SqlInteger Integer
  | SqlDouble Double
  | SqlText TL.Text
    -- | Blob field in the database. This field can not be implicitly converted
    -- to any other type because it is just an array of bytes, not an UTF-8
    -- encoded string.
  | SqlBlob B.ByteString
  | SqlBool Bool
    -- | Represent bit field with 64 bits
  | SqlBitField BitField
    -- | UUID value http://en.wikipedia.org/wiki/UUID
  | SqlUUID UUID

  | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
  | SqlLocalDate Day            -- ^ Local YYYY-MM-DD (no timezone)
  | SqlLocalTimeOfDay TimeOfDay -- ^ Local HH:MM:SS (no timezone)
  | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)
  | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
  deriving (Show, Typeable, Ord)

instance Eq SqlValue where

    (SqlDecimal a)        == (SqlDecimal b)         = a == b
    (SqlInteger a)        == (SqlInteger b)         = a == b
    (SqlDouble a)         == (SqlDouble b)          = a == b
    (SqlText a)           == (SqlText b)            = a == b
    (SqlBlob a)           == (SqlBlob b)            = a == b
    (SqlBool a)           == (SqlBool b)            = a == b
    (SqlBitField a)       == (SqlBitField b)        = a == b
    (SqlUUID a)           == (SqlUUID b)            = a == b
    (SqlUTCTime a)        == (SqlUTCTime b)         = a == b
    (SqlLocalDate a)      == (SqlLocalDate b)       = a == b
    (SqlLocalTimeOfDay a) == (SqlLocalTimeOfDay b)  = a == b
    (SqlLocalTime a)      == (SqlLocalTime b)       = a == b
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = case convres of    -- FIXME: uncomment
      Left _ -> False
      Right r -> r
      where
        convres = do
          (x :: String) <- safeFromSql a
          y <- safeFromSql b
          return $ x == y


instance ToSql Decimal where
  toSql = SqlDecimal

instance FromSql Decimal where
  safeFromSql (SqlDecimal d)          = Right d
  safeFromSql (SqlInteger i)          = Right $ fromIntegral i
  safeFromSql (SqlDouble d)           = Right $ realToFrac d
  safeFromSql (SqlText t)             = tryParse t $ signed rational
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Decimal)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = Right $ fromIntegral bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Decimal)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Decimal)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Decimal)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Decimal)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Decimal)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Decimal)


instance ToSql Int where
  toSql i = SqlInteger $ toInteger i

instance FromSql Int where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t $ signed decimal
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Int)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = convertToBounded $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Int)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Int)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Int)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Int)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Int)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Int)


instance ToSql Int32 where
  toSql i = SqlInteger $ toInteger i

instance FromSql Int32 where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t $ signed decimal
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Int32)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = convertToBounded $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Int32)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Int32)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Int32)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Int32)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Int32)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Int32)


instance ToSql Int64 where
  toSql i = SqlInteger $ toInteger i

instance FromSql Int64 where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t $ signed decimal
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Int64)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = convertToBounded $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Int64)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Int64)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Int64)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Int64)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Int64)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Int64)


instance ToSql Integer where
  toSql = SqlInteger

instance FromSql Integer where
  safeFromSql (SqlDecimal d)          = Right $ truncate d
  safeFromSql (SqlInteger i)          = Right i
  safeFromSql (SqlDouble d)           = Right $ truncate d
  safeFromSql (SqlText t)             = tryParse t $ signed decimal
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Integer)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = Right $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Integer)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Integer)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Integer)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Integer)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Integer)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Integer)


instance ToSql Word32 where
  toSql i = SqlInteger $ toInteger i

instance FromSql Word32 where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t (decimal <?> "Word32 parser")
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Word32)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = convertToBounded $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Word32)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Word32)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Word32)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Word32)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Word32)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Word32)


instance ToSql Word64 where
  toSql i = SqlInteger $ toInteger i

instance FromSql Word64 where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t (decimal <?> "Word64 parser")
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Word64)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = Right $ unBitField bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Word64)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Word64)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Word64)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Word64)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Word64)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Word64)


instance ToSql Word where
  toSql i = SqlInteger $ toInteger i

instance FromSql Word where
  safeFromSql (SqlDecimal d)          = convertToBounded $ truncate d
  safeFromSql (SqlInteger i)          = convertToBounded i
  safeFromSql (SqlDouble d)           = convertToBounded $ truncate d
  safeFromSql (SqlText t)             = tryParse t (decimal <?> "Word parser")
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Word)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = convertToBounded $ toInteger bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Word)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Word)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Word)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Word)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Word)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Word)


instance ToSql Double where
  toSql = SqlDouble

instance FromSql Double where
  safeFromSql (SqlDecimal d)          = Right $ realToFrac d
  safeFromSql (SqlInteger i)          = Right $ fromIntegral i
  safeFromSql (SqlDouble d)           = Right d
  safeFromSql (SqlText t)             = tryParse t $ signed double
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Double)
  safeFromSql (SqlBool b)             = Right $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = Right $ fromIntegral bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Double)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Double)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Double)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Double)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Double)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Double)


instance ToSql [Char] where
  toSql s = SqlText $ TL.pack s

instance FromSql [Char] where
  safeFromSql (SqlDecimal d)          = Right $ show d
  safeFromSql (SqlInteger i)          = Right $ show i
  safeFromSql (SqlDouble d)           = Right $ show d
  safeFromSql (SqlText t)             = Right $ TL.unpack t
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: String)
  safeFromSql (SqlBool b)             = Right $ if b then "t" else "f"
  safeFromSql (SqlBitField bf)        = Right $ formatBitField $ unBitField bf
  safeFromSql (SqlUUID u)             = Right $ toString u
  safeFromSql (SqlUTCTime ut)         = Right $ formatIsoUTCTime ut
  safeFromSql (SqlLocalDate ld)       = Right $ formatIsoDay ld
  safeFromSql (SqlLocalTimeOfDay tod) = Right $ formatIsoTimeOfDay tod
  safeFromSql (SqlLocalTime lt)       = Right $ formatIsoLocalTime lt
  safeFromSql SqlNull                 = nullConvertError (undefined :: String)


instance ToSql TL.Text where
  toSql = SqlText

instance FromSql TL.Text where
  safeFromSql (SqlText t) = Right t
  safeFromSql (SqlBlob b) = incompatibleTypes b (undefined :: TL.Text)
  safeFromSql SqlNull     = nullConvertError (undefined :: TL.Text)
  safeFromSql x           = TL.pack <$> safeFromSql x


instance ToSql T.Text where
  toSql t = SqlText $ TL.fromChunks [t]

instance FromSql T.Text where
  safeFromSql (SqlText t) = Right $ TL.toStrict t
  safeFromSql (SqlBlob b) = incompatibleTypes b (undefined :: T.Text)
  safeFromSql SqlNull     = nullConvertError (undefined :: T.Text)
  safeFromSql x           = T.pack <$> safeFromSql x


instance ToSql B.ByteString where
  toSql = SqlBlob

instance FromSql B.ByteString where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: B.ByteString)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: B.ByteString)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: B.ByteString)
  safeFromSql (SqlText t)             = incompatibleTypes t (undefined :: B.ByteString)
  safeFromSql (SqlBlob b)             = Right b
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: B.ByteString)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: B.ByteString)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: B.ByteString)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: B.ByteString)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: B.ByteString)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: B.ByteString)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: B.ByteString)
  safeFromSql SqlNull                 = nullConvertError (undefined :: B.ByteString)


instance ToSql BL.ByteString where
  toSql b = SqlBlob $ BB.toByteString $ BB.fromLazyByteString b

instance FromSql BL.ByteString where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: BL.ByteString)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: BL.ByteString)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: BL.ByteString)
  safeFromSql (SqlText t)             = incompatibleTypes t (undefined :: BL.ByteString)
  safeFromSql (SqlBlob b)             = Right $ BL.fromChunks [b]
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: BL.ByteString)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: BL.ByteString)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: BL.ByteString)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: BL.ByteString)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: BL.ByteString)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: BL.ByteString)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: BL.ByteString)
  safeFromSql SqlNull                 = nullConvertError (undefined :: BL.ByteString)


instance ToSql Bool where
  toSql = SqlBool

instance FromSql Bool where
  safeFromSql (SqlDecimal d)          = Right $ d /= 0
  safeFromSql (SqlInteger i)          = Right $ i /= 0
  safeFromSql (SqlDouble d)           = Right $ d /= 0
  safeFromSql (SqlText t)             = case TL.toLower t of
    "t"     -> Right True
    "true"  -> Right True
    "1"     -> Right True
    "f"     -> Right False
    "false" -> Right False
    "0"     -> Right False
    _       -> Left $ ConvertError
               $ "Could not convert string \"" ++ (show t) ++ "\" to Bool"
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Bool)
  safeFromSql (SqlBool b)             = Right b
  safeFromSql (SqlBitField bf)        = Right $ bf /= 0
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Bool)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Bool)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: Bool)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Bool)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: Bool)
  safeFromSql SqlNull                 = nullConvertError (undefined :: Bool)


instance ToSql BitField where
  toSql = SqlBitField

instance FromSql BitField where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: BitField)
  safeFromSql (SqlInteger i)          = BitField <$> convertToBounded i
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: BitField)
  safeFromSql (SqlText t)             = BitField <$> tryParse t parseBitField
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: BitField)
  safeFromSql (SqlBool b)             = Right $ BitField $ if b then 1 else 0
  safeFromSql (SqlBitField bf)        = Right bf
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: BitField)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: BitField)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: BitField)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: BitField)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: BitField)
  safeFromSql SqlNull                 = nullConvertError (undefined :: BitField)


instance ToSql UUID where
  toSql = SqlUUID

instance FromSql UUID where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: UUID)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: UUID)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: UUID)
  safeFromSql (SqlText t)             = case fromString $ TL.unpack t of
    Nothing -> Left $ ConvertError $ "Could not convert \"" ++ (show t) ++ "\" to UUID"
    Just u  -> Right u
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: UUID)
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: UUID)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: UUID)
  safeFromSql (SqlUUID u)             = Right u
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: UUID)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: UUID)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: UUID)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: UUID)
  safeFromSql SqlNull                 = nullConvertError (undefined :: UUID)


instance ToSql UTCTime where
  toSql = SqlUTCTime

instance FromSql UTCTime where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: UTCTime)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: UTCTime)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: UTCTime)
  safeFromSql (SqlText t)             = zonedTimeToUTC <$> tryParse t parseIsoZonedTime
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: UTCTime)
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: UTCTime)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: UTCTime)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: UTCTime)
  safeFromSql (SqlUTCTime ut)         = Right ut
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: UTCTime)
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: UTCTime)
  safeFromSql (SqlLocalTime lt)       = incompatibleTypes lt (undefined :: UTCTime)
  safeFromSql SqlNull                 = nullConvertError (undefined :: UTCTime)


instance ToSql Day where
  toSql = SqlLocalDate

instance FromSql Day where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: Day)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: Day)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: Day)
  safeFromSql (SqlText t)             = tryParse t parseIsoDay
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: Day)
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: Day)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: Day)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: Day)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: Day)
  safeFromSql (SqlLocalDate ld)       = Right $ ld
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: Day)
  safeFromSql (SqlLocalTime lt)       = Right $ localDay lt
  safeFromSql SqlNull                 = nullConvertError (undefined :: Day)


instance ToSql TimeOfDay where
  toSql = SqlLocalTimeOfDay

instance FromSql TimeOfDay where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: TimeOfDay)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: TimeOfDay)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: TimeOfDay)
  safeFromSql (SqlText t)             = tryParse t parseIsoTimeOfDay
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: TimeOfDay)
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: TimeOfDay)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: TimeOfDay)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: TimeOfDay)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: TimeOfDay)
  safeFromSql (SqlLocalDate ld)       = incompatibleTypes ld (undefined :: TimeOfDay)
  safeFromSql (SqlLocalTimeOfDay tod) = Right $ tod
  safeFromSql (SqlLocalTime lt)       = Right $ localTimeOfDay lt
  safeFromSql SqlNull                 = nullConvertError (undefined :: TimeOfDay)


instance ToSql LocalTime where
  toSql = SqlLocalTime

instance FromSql LocalTime where
  safeFromSql (SqlDecimal d)          = incompatibleTypes d (undefined :: LocalTime)
  safeFromSql (SqlInteger i)          = incompatibleTypes i (undefined :: LocalTime)
  safeFromSql (SqlDouble d)           = incompatibleTypes d (undefined :: LocalTime)
  safeFromSql (SqlText t)             = tryParse t parseIsoLocalTime
  safeFromSql (SqlBlob b)             = incompatibleTypes b (undefined :: LocalTime)
  safeFromSql (SqlBool b)             = incompatibleTypes b (undefined :: LocalTime)
  safeFromSql (SqlBitField bf)        = incompatibleTypes bf (undefined :: LocalTime)
  safeFromSql (SqlUUID u)             = incompatibleTypes u (undefined :: LocalTime)
  safeFromSql (SqlUTCTime ut)         = incompatibleTypes ut (undefined :: LocalTime)
  safeFromSql (SqlLocalDate ld)       = Right $ LocalTime ld midnight
  safeFromSql (SqlLocalTimeOfDay tod) = incompatibleTypes tod (undefined :: LocalTime)
  safeFromSql (SqlLocalTime lt)       = Right $ lt
  safeFromSql SqlNull                 = nullConvertError (undefined :: LocalTime)


instance (ToSql a) => ToSql (Maybe a) where
  toSql m = case m of
    Nothing -> SqlNull
    Just a  -> toSql a

instance (FromSql a) => FromSql (Maybe a) where
  safeFromSql SqlNull = Right Nothing
  safeFromSql x       = Just <$> safeFromSql x


instance ToSql SqlValue where
  toSql = id

instance FromSql SqlValue where
  safeFromSql x = Right x
  fromSql = id
