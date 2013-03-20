{-# LANGUAGE
    DeriveDataTypeable #-}

module Database.HDBC.SqlError
       (
         SqlError(..)
       , SqlResult
       ) where


import Control.Exception
import Control.Monad.Trans.Either
import Data.Typeable
{- | The main HDBC exception object.  As much information as possible
is passed from the database through to the application through this object.

Errors generated in the Haskell layer will have seNativeError set to -1.
-}

data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
              deriving (Eq, Show, Read, Typeable)

instance Exception SqlError


type SqlResult a = EitherT SqlError IO a
