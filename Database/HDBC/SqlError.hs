{-# LANGUAGE
    DeriveDataTypeable #-}

module Database.HDBC.SqlError
       (
         SqlError(..)
       ) where


import Control.Exception
import Data.Typeable
  
{- | The main HDBC exception object.  As much information as possible
is passed from the database through to the application through this object.

This is not for throwing an error but for exception handling with monad EitherT
-}

data SqlError = SqlError {seNativeError :: Int,
                          seErrorMsg :: String}
              deriving (Eq, Show, Read, Typeable)

instance Exception SqlError
