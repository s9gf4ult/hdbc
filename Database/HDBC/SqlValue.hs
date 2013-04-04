{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts

 #-}

#if ! (MIN_VERSION_time(1,1,3))
{-# LANGUAGE
    StandaloneDeriving #-} 
#endif

module Database.HDBC.SqlValue
    (
     -- * SQL value marshalling
     SqlValue(..)
    )

where
import Data.Dynamic
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Char(toUpper)
import Data.Word
import Data.Int
import Data.Decimal
import Data.UUID
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Convertible
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

quickError :: (Typeable a, Convertible SqlValue a) => SqlValue -> ConvertResult a
quickError sv = convError "incompatible types" sv



{- | 'SqlValue' is the main type for expressing Haskell values to SQL databases.

/WHAT IS SQLVALUE/

SqlValue is an intermediate type to store/recevie data to/from the
database. Every database driver will do it's best to properly convert any
SqlValue to the database record's field, and properly convert the record's field
to SqlValue back.

The 'SqlValue' has predefined 'Convertible' instances for many Haskell's
types. Any Haskell's type can be converted to the 'SqlValue' with
'Database.HDBC.Utils.toSql' function. There is no safeToSql function because
toSql never fails. Also, any 'SqlValue' type can be converted to almost any
Haskell's type as well. Not any 'SqlValue' can be converted back to Haskell's
type, so there is 'Database.HDBC.Utils.safeFromSql' function to do that
safely. There is unsafe 'Database.HDBC.Utils.toSql' function of caurse.

You can sure, that @fromSql . toSql == id@

/SQLVALUE CONSTRUCTORS/

SqlValue constructors is the MINIMAL set of constructors, required to represent
the most wide range of native database types.

For example, there is FLOAT native database type and DOUBLE, but any DOUBLE can
carry any FLOAT value, so there is no need to create 'SqlValue' constructor to
represent FLOAT type. 

/INTRODUCTION TO SQLVALUE/

This type is used to marshall Haskell data to and from database APIs.
HDBC driver interfaces will do their best to use the most accurate and
efficient way to send a particular value to the database server.

Values read back from the server are constructed with the most appropriate
'SqlValue' constructor.  'Database.HDBC.Utils.fromSql' or
'Database.HDBC.Utils.safeFromSql' can then be used to convert them into whatever
type is needed locally in Haskell.

Most people will use 'Database.HDBC.Utils.toSql' and
'Database.HDBC.Utils.fromSql' instead of manipulating 'SqlValue's directly.

/EASY CONVERSIONS BETWEEN HASKELL TYPES/

Conversions are powerful; for instance, you can call
'Database.HDBC.Utils.fromSql' on a SqlInt32 and get a String or a Double out of
it.  This class attempts to Do The Right Thing whenever possible, and will raise
an error when asked to do something incorrect or ambiguous.  In particular, when
converting to any type except a Maybe, 'SqlNull' as the input will cause an
error to be raised.

Conversions are implemented in terms of the "Data.Convertible" module, part of the
convertible package.  You can refer to its documentation, and import that module,
if you wish to parse the Left result from 'safeFromSql' yourself, or write your
own conversion instances.

Here are some notes about conversion:

 * Fractions of a second are not preserved on time values

 * There is no @safeToSql@ because 'Database.HDBC.Utils.toSql' never fails.

/ERROR CONDITIONS/

There may sometimes be an error during conversion.  For instance, if you have a
'SqlString' and are attempting to convert it to an Integer, but it doesn't parse
as an Integer, you will get an error.  This will be indicated as an exception if
using 'Database.HDBC.Utils.fromSql', or a Left result if using
'Database.HDBC.Utils.safeFromSql'.

/DETAILS ON SQL TYPES/


/TEXT AND BYTESTRINGS/


/EQUALITY OF SQLVALUE/

Two SqlValues are considered to be equal if one of these hold.  The
first comparison that can be made is controlling; if none of these
comparisons can be made, then they are not equal:

 * Both are NULL

 * Both represent the same type and the encapsulated values are considered equal
   by applying (==) to them

 * The values of each, when converted to a string, are equal.

-}
data SqlValue =
  {- | Arbitrary precision DECIMAL value -}
  SqlDecimal Decimal
  | SqlWord32 Word32
  | SqlWord64 Word64
  | SqlInt32 Int32
  | SqlInt64 Int64
  | SqlInteger Integer
  | SqlDouble Double
  | SqlString String
  | SqlByteString B.ByteString
  | SqlBool Bool
    {- | Represent bit field with 64 bits -}
  | SqlBitField Word64
    {- | UUID value http://en.wikipedia.org/wiki/UUID -}
  | SqlUUID UUID

  | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
  | SqlLocalDate Day            -- ^ Local YYYY-MM-DD (no timezone)
  | SqlLocalTimeOfDay TimeOfDay -- ^ Local HH:MM:SS (no timezone)
  | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)

    {- | The value of current datetime on the server.
      Different database drivers will convert it to the appropriate literal/function.
      Not used for retriving data from the database, just for writing to.
    -}
  | SqlNow
  | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
  deriving (Show, Typeable)

instance Eq SqlValue where

    (SqlDecimal a)        == (SqlDecimal b)         = a == b
    (SqlWord32 a)         == (SqlWord32 b)          = a == b
    (SqlWord64 a)         == (SqlWord64 b)          = a == b
    (SqlInt32 a)          == (SqlInt32 b)           = a == b
    (SqlInt64 a)          == (SqlInt64 b)           = a == b
    (SqlInteger a)        == (SqlInteger b)         = a == b
    (SqlDouble a)         == (SqlDouble b)          = a == b
    (SqlString a)         == (SqlString b)          = a == b
    (SqlByteString a)     == (SqlByteString b)      = a == b
    (SqlBool a)           == (SqlBool b)            = a == b
    (SqlBitField a)       == (SqlBitField b)        = a == b
    (SqlUUID a)           == (SqlUUID b)            = a == b
    (SqlUTCTime a)        == (SqlUTCTime b)         = a == b
    (SqlLocalDate a)      == (SqlLocalDate b)       = a == b
    (SqlLocalTimeOfDay a) == (SqlLocalTimeOfDay b)  = a == b
    (SqlLocalTime a)      == (SqlLocalTime b)       = a == b
    SqlNow == SqlNow = False     -- Concrete value will be determined on the database
    _ == SqlNow = False
    SqlNow == _ = False
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = case convres of
      Left _ -> False
      Right r -> r
      where
        convres = do
          x <- (safeConvert a) :: ConvertResult String
          y <- (safeConvert b) :: ConvertResult String
          return $ x == y

instance Convertible SqlValue SqlValue where
    safeConvert = return

instance Convertible [Char] SqlValue where
    safeConvert = return . SqlString
instance Convertible SqlValue [Char] where

  safeConvert (SqlDecimal a)        = return $ show a
  safeConvert (SqlWord32 a)         = return $ show a
  safeConvert (SqlWord64 a)         = return $ show a
  safeConvert (SqlInt32 a)          = return $ show a
  safeConvert (SqlInt64 a)          = return $ show a
  safeConvert (SqlInteger a)        = return $ show a
  safeConvert (SqlDouble a)         = return $ show a
  safeConvert (SqlString a)         = return a
  safeConvert (SqlByteString x)     = return . BUTF8.toString $ x
  safeConvert (SqlBool a)           = return $ show a
  safeConvert (SqlBitField a)       = return $ show a
  safeConvert (SqlUUID a)           = return $ show a
  safeConvert (SqlUTCTime a)        = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert (SqlLocalDate a)      = return . formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ a
  safeConvert (SqlLocalTimeOfDay a) = return . formatTime defaultTimeLocale "%T%Q" $ a
  safeConvert (SqlLocalTime a)      = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert x@SqlNow  = quickError x
  safeConvert x@SqlNull = quickError x

instance Convertible TS.Text SqlValue where
    safeConvert = return . SqlString . TS.unpack

instance Convertible SqlValue TS.Text where
    safeConvert = fmap TS.pack . safeConvert

instance Convertible TL.Text SqlValue where
    safeConvert = return . SqlString . TL.unpack

instance Convertible SqlValue TL.Text where
    safeConvert = fmap TL.pack . safeConvert


instance Convertible B.ByteString SqlValue where
    safeConvert = return . SqlByteString
instance Convertible SqlValue B.ByteString where
    safeConvert (SqlByteString x) = return x
    safeConvert y@SqlNull = quickError y
    safeConvert x = safeConvert x >>= return . BUTF8.fromString

instance Convertible BSL.ByteString SqlValue where
    safeConvert = return . SqlByteString . B.concat . BSL.toChunks
instance Convertible SqlValue BSL.ByteString where
    safeConvert x = do bs <- safeConvert x
                       return (BSL.fromChunks [bs])

instance Convertible Int SqlValue where
    safeConvert x = fmap SqlInt64 $ safeConvert x
instance Convertible SqlValue Int where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int32 SqlValue where
    safeConvert = return . SqlInt32
instance Convertible SqlValue Int32 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = return a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int64 SqlValue where
    safeConvert = return . SqlInt64
instance Convertible SqlValue Int64 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = return a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Word32 SqlValue where
    safeConvert = return . SqlWord32
instance Convertible SqlValue Word32 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = return a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Word64 SqlValue where
    safeConvert = return . SqlWord64
instance Convertible SqlValue Word64 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = return a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = return a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Integer SqlValue where
    safeConvert = return . SqlInteger
instance Convertible SqlValue Integer where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = return a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Bool SqlValue where
    safeConvert = return . SqlBool
instance Convertible SqlValue Bool where
  safeConvert (SqlDecimal a)          = numToBool a
  safeConvert (SqlWord32 a)           = numToBool a
  safeConvert (SqlWord64 a)           = numToBool a
  safeConvert (SqlInt32 a)            = numToBool a
  safeConvert (SqlInt64 a)            = numToBool a
  safeConvert (SqlInteger a)          = numToBool a
  safeConvert (SqlDouble a)           = numToBool a
  safeConvert y@(SqlString x) =
    case map toUpper x of
      "TRUE" -> Right True
      "T" -> Right True
      "FALSE" -> Right False
      "F" -> Right False
      "0" -> Right False
      "1" -> Right True
      _ -> convError "Cannot parse given String as Bool" y
  safeConvert (SqlByteString x)       = (safeConvert . SqlString . BUTF8.toString) x
  safeConvert (SqlBool a)             = return a
  safeConvert (SqlBitField a)         = numToBool a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Bool has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

numToBool :: (Eq a, Num a) => a -> ConvertResult Bool
numToBool x = Right (x /= 0)

instance Convertible Double SqlValue where
    safeConvert = return . SqlDouble
instance Convertible SqlValue Double where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = return a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Decimal SqlValue where
  safeConvert = return . SqlDecimal
instance Convertible SqlValue Decimal where
  safeConvert (SqlDecimal a)          = return a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y



#if ! (MIN_VERSION_time(1,1,3))
    -- older versions if time had no Typeable instances
deriving instance Typeable Day
deriving instance Typeable TimeOfDay
deriving instance Typeable LocalTime
#endif

instance Convertible Day SqlValue where
    safeConvert = return . SqlLocalDate
instance Convertible SqlValue Day where
  safeConvert x@(SqlDecimal _)                   = quickError x -- converting number to date has no sense
  safeConvert x@(SqlWord32 _)                    = quickError x
  safeConvert x@(SqlWord64 _)                    = quickError x
  safeConvert x@(SqlInt32 _)                     = quickError x
  safeConvert x@(SqlInt64 _)                     = quickError x
  safeConvert x@(SqlInteger _)                   = quickError x
  safeConvert x@(SqlDouble _)                    = quickError x
  safeConvert (SqlString x)                      = parseTime' (iso8601DateFormat Nothing) x
  safeConvert (SqlByteString x)                  = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)                      = quickError x
  safeConvert x@(SqlBitField _)                  = quickError x
  safeConvert x@(SqlUUID _)                      = quickError x
  safeConvert x@(SqlUTCTime _)                   = quickError x -- converting UTC time to local day has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate a)                   = return a
  safeConvert x@(SqlLocalTimeOfDay _)            = quickError x
  safeConvert (SqlLocalTime (LocalTime {localDay = a})) = return a
  safeConvert x@SqlNow                           = quickError x
  safeConvert y@(SqlNull)                        = quickError y

instance Convertible TimeOfDay SqlValue where
    safeConvert = return . SqlLocalTimeOfDay
instance Convertible SqlValue TimeOfDay where
  safeConvert x@(SqlDecimal _)                         = quickError x -- converting number to time has no sense
  safeConvert x@(SqlWord32 _)                          = quickError x
  safeConvert x@(SqlWord64 _)                          = quickError x
  safeConvert x@(SqlInt32 _)                           = quickError x
  safeConvert x@(SqlInt64 _)                           = quickError x
  safeConvert x@(SqlInteger _)                         = quickError x
  safeConvert x@(SqlDouble _)                          = quickError x
  safeConvert (SqlString x)                            = parseTime' "%T%Q" x
  safeConvert (SqlByteString x)                        = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)                            = quickError x
  safeConvert x@(SqlBitField _)                        = quickError x
  safeConvert x@(SqlUUID _)                            = quickError x
  safeConvert x@(SqlUTCTime _)                         = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert x@(SqlLocalDate _)                       = quickError x
  safeConvert (SqlLocalTimeOfDay a)                    = return a
  safeConvert (SqlLocalTime (LocalTime {localTimeOfDay = a})) = return a
  safeConvert x@SqlNow                                 = quickError x
  safeConvert y@(SqlNull)                              = quickError y

instance Convertible LocalTime SqlValue where
    safeConvert = return . SqlLocalTime
instance Convertible SqlValue LocalTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to time of day has no sense
  safeConvert x@(SqlWord32 _)         = quickError x
  safeConvert x@(SqlWord64 _)         = quickError x
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlString x)           = parseTime' (iso8601DateFormat (Just "%T%Q")) x
  safeConvert (SqlByteString x)       = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert x@(SqlUTCTime _)        = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate d)        = return $ LocalTime d midnight
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert (SqlLocalTime a)        = return a
  safeConvert x@SqlNow                = quickError x
  safeConvert y@SqlNull               = quickError y

instance Convertible UTCTime SqlValue where
    safeConvert = return . SqlUTCTime
instance Convertible SqlValue UTCTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to UTC has no sense
  safeConvert x@(SqlWord32 _)         = quickError x
  safeConvert x@(SqlWord64 _)         = quickError x
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlString x)           = parseTime' (iso8601DateFormat (Just "%T%Q")) x
  safeConvert (SqlByteString x)       = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert (SqlUTCTime a)          = return a
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@SqlNull               = quickError y

instance (Convertible a SqlValue) => Convertible (Maybe a) SqlValue where
    safeConvert Nothing = return SqlNull
    safeConvert (Just a) = safeConvert a
instance (Convertible SqlValue a) => Convertible SqlValue (Maybe a) where
    safeConvert SqlNull = return Nothing
    safeConvert a = safeConvert a >>= (return . Just)

-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Typeable a, Read a, Convertible SqlValue a) => String -> ConvertResult a
read' s =
    case [x | (x, "") <- reads s] of
      [x] -> Right x
      _ -> convError "Cannot read source value as dest type" (SqlString s)

#ifdef __HUGS__
parseTime' :: (Typeable t, Convertible SqlValue t) => String -> String -> ConvertResult t
parseTime' _ inpstr =
    convError "Hugs does not support time parsing" (SqlString inpstr)
#else
parseTime' :: (Typeable t, Convertible SqlValue t, ParseTime t) => String -> String -> ConvertResult t
parseTime' fmtstr inpstr =
    case parseTime defaultTimeLocale fmtstr inpstr of
      Nothing -> convError ("Cannot parse using default format string " ++ show fmtstr)
                 (SqlString inpstr)
      Just x -> Right x
#endif

-- | As the semantic of System.Locale.iso8601DateFormat has changed with
--   old-locale-1.0.0.2 in a non-compatible way, we now define our own
--   (compatible) version of it.
iso8601DateFormat :: Maybe String -> String
iso8601DateFormat mTimeFmt =
    "%Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> ' ' : fmt
