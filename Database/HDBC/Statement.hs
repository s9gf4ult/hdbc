
module Database.HDBC.Statement
       (
         Statement(..)
       )  where
  
import Database.HDBC.SqlValue (SqlValue)
import Database.HDBC.SqlError

data Statement = Statement
    {
     {- | Execute the prepared statement, passing in the given positional
        parameters (that should take the place of the question marks
        in the call to 'prepare').

        For non-SELECT queries, the return value is the number of
        rows modified, if known.  If no rows were modified, you get 0.
        If the value is unknown, you get -1.  All current HDBC drivers
        support this function and should never return -1.

        For SELECT queries, you will always get 0.

        This function should automatically call finish() to finish the previous
        execution, if necessary.
        -}
     execute :: [SqlValue] -> SqlResult (),

     {- | Execute the statement as-is, without supplying any
        positional parameters.  This is intended for statements for
        which the results aren't interesting or present (e.g., DDL or
        DML commands).  If your query contains placeholders, this will
        certainly fail; use 'execute' instead. -}
     executeRaw :: SqlResult (),

     {- | Execute the query with many rows. 
        The return value is the return value from the final row 
        as if you had called 'execute' on it.

        Due to optimizations that are possible due to different
        databases and driver designs, this can often be significantly
        faster than using 'execute' multiple times since queries
        need to be compiled only once.

        This is most useful for non-SELECT statements. -}
     executeMany :: [[SqlValue]] -> SqlResult (),
     
     {-| Return count of affected by query rows, return 0 if query was
         SELECT 
      -}
     affectedRows :: SqlResult Integer,

                 
     {- | Abort a query in progress -- usually not needed. -}
     finish :: SqlResult (),

     {- | Fetches one row from the DB.  Returns 'Nothing' if there
        are no more rows.  Will automatically call 'finish' when
        the last row is read. -}
     fetchRow :: SqlResult (Maybe [SqlValue]),

     {- | Returns a list of the column names in the result.
        For maximum portability, you should not assume that
        information is available until after an 'execute' function
        has been run.
        
        Information is returned here directly as returned
        by the underlying database layer.  Note that different
        databases have different rules about capitalization
        of return values and about representation of names
        of columns that are not simple columns.  For this reason,
        it is suggested that you treat this information for
        display purposes only.  Failing that, you should convert
        to lower (or upper) case, and use @AS@ clauses for
        anything other than simple columns.

        A simple getColumnNames implementation could simply
        apply @map fst@ to the return value of 'describeResult'.
        -}
     getColumnNames :: SqlResult [String],


     {- | The original query that this 'Statement' was prepared
          with. -}
     originalQuery :: String
    }

