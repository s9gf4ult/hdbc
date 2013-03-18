{-# LANGUAGE
    DeriveDataTypeable #-}

module Database.HDBC.SqlError
       (
         SqlError(..)
       , SqlResult
       ) where


import Control.Exception
import Data.Typeable
{- | The main HDBC exception object.  As much information as possible
is passed from the database through to the application through this object.

Errors generated in the Haskell layer will have seNativeError set to -1.
-}

data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
              deriving (Eq, Show, Read, Typeable)

#if __GLASGOW_HASKELL__ >= 610
--data SqlException
instance Exception SqlError
{-
    toException = SomeException
    fromException (SomeException e) = Just e
    fromException _ = Nothing
-}
#endif


type SqlResult a = Either SqlError a
