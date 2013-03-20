{-# LANGUAGE
    DeriveDataTypeable #-}

module Database.HDBC.Statement
       (
         Statement(..)
       , StatementStatus(..)
       , StmtWrapper(..)
       )  where

import Database.HDBC.SqlValue (SqlValue)
import Database.HDBC.SqlError

import Data.Typeable

data StatementStatus = StatementNew      -- ^ Newly created statement
                     | StatementExecuted -- ^ Expression executed
                     | StatementFinished -- ^ Finished, fetching rows will return 'Nothing'

class (Typeable stmt) => Statement stmt where
  execute :: stmt -> [SqlValue] -> SqlResult ()
  executeRaw :: stmt -> SqlResult ()
  executeMany :: stmt -> [[SqlValue]] -> SqlResult ()
  statementStatus :: stmt -> SqlResult StatementStatus
  affectedRows :: stmt -> SqlResult Integer
  finish :: stmt -> SqlResult ()
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
