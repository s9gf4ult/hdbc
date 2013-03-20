{-# LANGUAGE
    CPP
  , TypeFamilies
  , DeriveDataTypeable #-}

{- |
   Module     : Database.HDBC.Utils
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Internal module -- not exported directly.

Everything in here is expoerted by "Database.HDBC".  Please use -- and read --
"Database.HDBC" directly.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Utils
       (
         -- * Exception handling
         catchSql
       , handleSql
       , sqlExceptions
       , handleSqlError
       , throwSqlError
       , sqlBracket
         -- * Convertible helpers
       , toSql
       , safeFromSql
       , fromSql
       , nToSql
       , iToSql
         -- * Transaction handling
       , withTransaction
         -- * Query execution helpers
       , withStatement
       , run
       , runRaw
       , runMany
       ) where
import Prelude hiding (catch)

import Control.Monad.Trans.Either (EitherT (..))
  
import Database.HDBC.Connection
import Database.HDBC.Statement
import Database.HDBC.SqlValue
import Database.HDBC.SqlError
import Control.Exception (bracket, catchJust, onException, catch, throw, SomeException(..))
import Data.Convertible

{- | Execute the given IO action.

If it raises a 'SqlError', then execute the supplied handler and return its
return value.  Otherwise, proceed as normal. -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql action handler = 
    catchJust sqlExceptions action handler

{- | Like 'catchSql', with the order of arguments reversed. -}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql h f = catchSql f h

{- | Given an Exception, return Just SqlError if it was an SqlError, or Nothing
otherwise. Useful with functions like catchJust. -}
sqlExceptions :: SqlError -> Maybe SqlError
sqlExceptions e = Just e


{- | Catches 'SqlError's, and re-raises them as IO errors with fail.
Useful if you don't care to catch SQL errors, but want to see a sane
error message if one happens.  One would often use this as a high-level
wrapper around SQL calls. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

throwSqlError :: SqlResult a -> IO a
throwSqlError m = do
  res <- runEitherT m
  case res of
    Right a -> return a
    Left  e -> throw e
    
{- | Convert a value to an 'SqlValue'.  This function is simply
a restricted-type wrapper around 'convert'.  See extended notes on 'SqlValue'. -}
toSql :: Convertible a SqlValue => a -> SqlValue
toSql = convert

{- | Conversions to and from 'SqlValue's and standard Haskell types.

This function converts from an 'SqlValue' to a Haskell value.  Many people will use the simpler
   'fromSql' instead.  This function is simply a restricted-type wrapper around
   'safeConvert'. -}
safeFromSql :: Convertible SqlValue a => SqlValue -> ConvertResult a
safeFromSql = safeConvert

{- | Convert from an 'SqlValue' to a Haskell value.  Any problem is indicated by
   calling 'error'.  This function is simply a restricted-type wrapper around
   'convert'.  See extended notes on 'SqlValue'. -}
fromSql :: Convertible SqlValue a => SqlValue -> a
fromSql = convert

{- | Converts any Integral type to a 'SqlValue' by using toInteger. -}
nToSql :: Integral a => a -> SqlValue
nToSql n = SqlInteger (toInteger n)

{- | Convenience function for using numeric literals in your program. -}
iToSql :: Int -> SqlValue
iToSql = toSql


{- | Execute some code.  If any uncaught exception occurs, run
'rollback' and re-raise it.  Otherwise, run 'commit' and return.

This function, therefore, encapsulates the logical property that a transaction
is all about: all or nothing.

The 'IConnection' object passed in is passed directly to the specified
function as a convenience.

This function traps /all/ uncaught exceptions, not just SqlErrors.  Therefore,
you will get a rollback for any exception that you don't handle.  That's
probably what you want anyway.

Since all operations in HDBC are done in a transaction, this function doesn't
issue an explicit \"begin\" to the server.  You should ideally have
called 'Database.HDBC.commit' or 'Database.HDBC.rollback' before
calling this function.  If you haven't, this function will commit or rollback
more than just the changes made in the included action.

If there was an error while running 'rollback', this error will not be
reported since the original exception will be propogated back.  (You'd probably
like to know about the root cause for all of this anyway.)  Feedback
on this behavior is solicited.
-}
withTransaction :: Connection conn => conn -> (conn -> IO a) -> IO a
withTransaction conn func = do
  r <- onException (func conn) doRollback
  throwSqlError $ commit conn
  return r
  where
    doRollback :: IO ()
    doRollback = 
              -- Discard any exception from (rollback conn) so original
              -- exception can be re-raised
      catch (throwSqlError $ rollback conn) doRollbackHandler
    doRollbackHandler :: SomeException -> IO ()
    doRollbackHandler _ = return ()

{-| Create statement and execute monadic action using 
it. Safely finalize Statement after action is done.
-}
withStatement :: (Connection conn, Statement stmt, stmt ~ (ConnStatement conn))
                 => conn                 -- ^ Connection
                 -> String               -- ^ Query string
                 -> (stmt -> SqlResult a) -- ^ Action around statement
                 -> SqlResult a          -- ^ Result of action
withStatement conn query = sqlBracket
                           (prepare conn query)
                           finish

sqlBracket :: (SqlResult a) -> (a -> SqlResult b) -> (a -> SqlResult c) -> SqlResult c
sqlBracket aloc dealoc action = EitherT $ bracket ioAlloc ioDealloc ioAction
  where
    ioAlloc = runEitherT aloc
    ioDealloc = \a -> runEitherT $ (EitherT . return $ a) >>= dealoc
    ioAction = \a -> runEitherT $ (EitherT . return $ a) >>= action

{-| Run query and safely finalize statement after that
-}
run :: (Connection conn) => conn -> String -> [SqlValue] -> SqlResult ()
run conn query values = withStatement conn query $
                        \s -> execute s values

  
{-| Run raw query without parameters and safely finalize
statement
-}
runRaw :: (Connection conn) => conn -> String -> SqlResult ()
runRaw conn query = withStatement conn query executeRaw
  
{-| run executeMany and safely finalize statement -}
runMany :: (Connection conn) => conn -> String -> [[SqlValue]] -> SqlResult ()
runMany conn query values = withStatement conn query $
                            \s -> executeMany s values
