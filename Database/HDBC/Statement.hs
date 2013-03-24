{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
 #-}

module Database.HDBC.Statement
       (
         Statement(..)
       , StatementStatus(..)
       , StmtWrapper(..)
       )  where

import Database.HDBC.SqlValue (SqlValue)
import Database.HDBC.SqlError

import Data.Typeable

-- | Statement's status returning by function 'statementStatus'.
data StatementStatus = StatementNew      -- ^ Newly created statement
                     | StatementExecuted -- ^ Expression executed
                     | StatementFinished -- ^ Finished, fetching rows will return 'Nothing'

                       
class (Typeable stmt) => Statement stmt where

  -- | Execute single query with parameters. In query each parameter must be
  -- replaced with ''?'' placeholder. This rule is true for every database, even
  -- for PostgreSQL which uses placeholders like ''$1''. Application must ensure
  -- that the count of placeholders is equal to count of parameter, it is likely
  -- cause an error if it is not.
  execute :: stmt -> [SqlValue] -> SqlResult ()

  -- | Execute single query without parameters. Has default implementation
  -- through 'execute'.
  executeRaw :: stmt -> SqlResult ()
  executeRaw stmt = execute stmt []

  -- | Execute one query many times with a list of paramters. Has default
  -- implementation through 'execute'.
  executeMany :: stmt -> [[SqlValue]] -> SqlResult ()
  executeMany stmt vals = mapM_ (execute stmt) vals

  -- | Return the current statement's status.
  statementStatus :: stmt -> SqlResult StatementStatus

  -- | Return the count of rows affected by INSERT, UPDATE or DELETE
  -- query. After executing SELECT query it will return 0 every time.
  affectedRows :: stmt -> SqlResult Integer

  -- | Finish statement and remove database-specific pointer. No any actions may
  -- be proceeded after closing the statement, excpet 'statementStatus' which
  -- will return 'StatementFinished'.
  finish :: stmt -> SqlResult ()

  -- | Reset statement to initial state.
  reset :: stmt -> SqlResult ()

  
  fetchRow :: stmt -> SqlResult (Maybe [SqlValue])
  
  getColumnNames :: stmt -> SqlResult [String]
  
  originalQuery :: stmt -> String

data StmtWrapper = forall stmt. Statement stmt => StmtWrapper stmt
                   deriving (Typeable)

instance Statement StmtWrapper where
  execute (StmtWrapper stmt) = execute stmt
  executeRaw (StmtWrapper stmt) = executeRaw stmt
  executeMany (StmtWrapper stmt) = executeMany stmt
  statementStatus (StmtWrapper stmt) = statementStatus stmt
  affectedRows (StmtWrapper stmt) = affectedRows stmt
  finish (StmtWrapper stmt) = finish stmt
  fetchRow (StmtWrapper stmt) = fetchRow stmt
  getColumnNames (StmtWrapper stmt) = getColumnNames stmt
  originalQuery (StmtWrapper stmt) = originalQuery stmt
