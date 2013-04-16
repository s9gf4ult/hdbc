{-# LANGUAGE
    DeriveDataTypeable #-}

module Database.HDBC.SqlError
       (
         SqlError(..)
       -- , SqlResult
       ) where


import Control.Exception
import Control.Monad.Trans.Either
import Data.Typeable
{- | The main HDBC exception object.  As much information as possible
is passed from the database through to the application through this object.

This is not for throwing an error but for exception handling with monad EitherT
-}

data SqlError = SqlError {seNativeError :: Int,
                          seErrorMsg :: String}
              deriving (Eq, Show, Read, Typeable)

instance Exception SqlError
                       
-- | The value which is result of any operation with database.
--
-- As long as any C function does not fail, but just return non-zero result to
-- indicate exception there is no sense in doing fail in haskell layer. We just
-- return EitherT which is composable and convenient to work with.
                       
-- type SqlResult a = EitherT SqlError IO a
