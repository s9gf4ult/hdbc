module Database.HDBI.Formaters
       (
         formatBitField
       , formatIsoUTCTime
       , formatIsoDay
       , formatIsoTimeOfDay
       , formatIsoLocalTime
       ) where 

import Data.Word
import Data.Bits
import Data.Time
import System.Locale (defaultTimeLocale)


formatBitField :: Word64 -> String
formatBitField w = "b'" ++ (map (tochar . testBit w) [bs,bs-1..0]) ++ "'"
  where
    bs = (bitSize w) - 1
    tochar True = '1'
    tochar False = '0'


formatIsoUTCTime :: UTCTime -> String
formatIsoUTCTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z"

formatIsoDay :: Day -> String
formatIsoDay = formatTime defaultTimeLocale "%Y-%m-%d"

formatIsoTimeOfDay :: TimeOfDay -> String
formatIsoTimeOfDay = formatTime defaultTimeLocale "%H:%M:%S%Q"

formatIsoLocalTime :: LocalTime -> String
formatIsoLocalTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"
