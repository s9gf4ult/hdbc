{-# LANGUAGE
  OverloadedStrings
  #-}

module Database.HDBC.Parsers
       (
-- | This module is for driver developers. Here is fast parsers, trying to parse
-- time and date in the most wide input formats.
         
-- * Parsers         
         parseBitField
       , parseIsoZonedTime
       , parseIsoDay
       , parseIsoTimeOfDay
       , parseIsoLocalTime
       ) where

import Control.Applicative ((<$>), (<|>))
import qualified Data.Attoparsec.Text.Lazy as P
import Data.Bits
import Data.Time
import Data.Word
import Data.Monoid (getFirst, First(..), mconcat)
import qualified Data.Text as T

spaces :: P.Parser ()
spaces = P.takeWhile (\x -> x == ' ' || x == '\t') >> return ()


-- | Parse bit field literal in format ''b'00101011'''. Takes just last 64 bits
-- of input, other bits are ignored
parseBitField :: P.Parser Word64
parseBitField = do
  _ <- P.string "b'"
  d <- P.takeWhile $ \x -> x == '0' || x == '1'
  _ <- P.string "'"
  return $ toword d
  where
    toword t = foldl setBit 0
               $ map snd
               $ filter fst
               $ zip (take wlen
                      $ reverse
                      $ map tobool
                      $ T.unpack t) [0..]
    wlen = bitSize (undefined :: Word64)
    tobool '1' = True
    tobool '0' = False
    tobool  _  = error "tobool got wrong value, error in the parser, please report a bug"

parseIsoZonedTime :: P.Parser ZonedTime
parseIsoZonedTime = zoned P.<?> "ZonedTime parser"
  where
    zoned = do
      time          <- parseIsoLocalTime
      spaces
      (addt, z) <- zone
      return $ if addt /= 0
               then utcToZonedTime z $ addUTCTime addt $ zonedTimeToUTC $ ZonedTime time z
               else ZonedTime time z
    zone = do
      sign <- P.option '+' (P.char '-' <|> P.char '+')
      (a, z) <- hhmmss <|> hhmm <|> hhhh
      return $ if sign == '+'
               then (a, minutesToTimeZone z)
               else (negate a, minutesToTimeZone $ negate z)

    hhmmss = do
      hh <- P.decimal
      _ <- P.char ':'
      mm <- P.decimal
      _ <- P.char ':'
      ss <- P.decimal
      return (fromInteger ss, mm + (60 * hh))

    hhmm = do
      hh <- P.decimal
      _ <- P.char ':'
      mm <- P.decimal
      return (0, mm + (60 * hh))

    hhhh = do
      hh <- P.decimal
      return (0, hh * 60)

parseIsoDay :: P.Parser Day
parseIsoDay = dayparse P.<?> "Day parser"
  where
    dayparse = do
      yr <- P.decimal
      delim
      mn <- P.decimal
      delim
      dy <- P.decimal
      let err = getFirst $ mconcat
                [ First $ if mn > 12 || mn < 1
                          then Just $ "month is " ++ show mn ++ " must be in bounds from 1 to 12"
                          else Nothing
                , First $ if dy > 31 || dy < 1
                          then Just $ "day is " ++ show dy ++ " must be in bounds from 1 to 31"
                          else Nothing
                ]
      case err of
        Just e -> fail e
        Nothing -> case fromGregorianValid yr mn dy of
          Just ret -> return ret
          Nothing -> fail $ "could not convert year: " ++ show yr
                     ++ " month: " ++ show mn
                     ++ " day: " ++ show dy
                     ++ " to date"

    delim = do
      spaces
      _ <- P.option Nothing $ Just <$> do
        _ <- P.char '-'
        spaces
      return ()
      


parseIsoTimeOfDay :: P.Parser TimeOfDay
parseIsoTimeOfDay = timeparse P.<?> "TimeOfDay parser"
  where
    timeparse = do
      hh <- P.decimal P.<?> "hours"
      colon
      mm <- P.decimal P.<?> "minutes"
      colon
      ss <- P.rational P.<?> "seconds"
      let err = getFirst $ mconcat
                [ First $ if hh > 23 || hh < 0
                          then Just $ "Hour is " ++ show hh ++ " must be in bounds from 0 to 23"
                          else Nothing
                , First $ if mm > 59 || hh < 0
                          then Just $ "Minute is " ++ show mm ++ " must be in bounds from 0 to 59"
                          else Nothing
                , First $ if ss > 60 || ss < 0
                          then Just $ "Seconds is " ++ show ss ++ " must be in bounds from 0 to 59"
                          else Nothing
                ]
      case err of
        Nothing -> return $ TimeOfDay hh mm ss
        Just e  -> fail e

    colon = do
      spaces
      _ <- P.char ':'
      spaces
      return ()

parseIsoLocalTime :: P.Parser LocalTime
parseIsoLocalTime = parsetime P.<?> "LocalTime parser"
  where
    parsetime = do
      day <- parseIsoDay
      spaces
      _ <- P.option Nothing $ Just <$> do
        _ <- P.char 'T'
        spaces
      time <- parseIsoTimeOfDay
      return $ LocalTime day time
