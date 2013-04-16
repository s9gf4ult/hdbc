{-# LANGUAGE
    CPP
  , TypeFamilies
  , DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
 #-}

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
         -- sqlBracket
         -- * Convertible helpers
         toSql
       , safeFromSql
       , fromSql
         -- * Transaction handling
       , withTransaction
         -- * Query execution helpers
       , withStatement
       , run
       , runRaw
       , runMany
       ) where
import Prelude hiding (catch)

import qualified Data.Text.Lazy as TL
  
import Database.HDBC.Connection
import Database.HDBC.Statement
import Database.HDBC.SqlValue
import Control.Exception (bracket, try, throwIO, catch, SomeException(..))
import Data.Convertible

-- throwSqlError :: IO a -> IO a
-- throwSqlError m = do
--   res <- runEitherT m
--   case res of
--     Right a -> return a
--     Left  e -> throw e
    
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
  r <- try (func conn)
  case r of
    Right x -> do
      commit conn
      return x
    Left (e :: SomeException) -> do
      catch  (rollback conn) (\(_ :: SomeException) -> return ())
      throwIO e

{-| Create statement and execute monadic action using 
it. Safely finalize Statement after action is done.
-}
withStatement :: (Connection conn, Statement stmt, stmt ~ (ConnStatement conn))
                 => conn          -- ^ Connection
                 -> TL.Text       -- ^ Query string
                 -> (stmt -> IO a) -- ^ Action around statement
                 -> IO a          -- ^ Result of action
withStatement conn query = bracket
                           (prepare conn query)
                           finish


  
