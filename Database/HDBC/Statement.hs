
module Database.HDBC.Statement
       (
         Statement(..)
       , StatementStatus(..)
       )  where
  
import Database.HDBC.SqlValue (SqlValue)
import Database.HDBC.SqlError

import Data.Typeable

data StatementStatus = StmtNew      -- ^ Newly created statement
                     | StmtExecuted -- ^ Expression executed
                     | StmtFinished -- ^ Finished, fetching rows will return 'Nothing'

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

