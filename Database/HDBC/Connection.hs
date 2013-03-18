{- |
   Module     : Database.HDBC.Types
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Types for HDBC.

Please note: this module is intended for authors of database driver libraries
only.  Authors of applications using HDBC should use 'Database.HDBC'
exclusively.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Connection
       (
         IConnection(..)
       ) where

import Database.HDBC.Statement (Statement)
import Database.HDBC.SqlError

{- | Main database handle object.

An 'IConnection' object is created by specific functions in the module for an
individual database.  That is, the connect function -- which creates
this object -- is not standardized through the HDBC interface.

A connection is closed by a call to 'disconnect'.

A call to 'commit' is required to make sure that your changes get committed
to the database.  In other words, HDBC has /no support for autocommit/, which
we consider an outdated notion.
-}
class IConnection conn where
                {- | Disconnect from the remote database.

You do not need to explicitly close an IConnection object, but you may do so if
you so desire.  If you don't, the object will disconnect from the database
in a sane way when it is garbage-collected.  However, a disconnection may
raise an error, so you are encouraged to explicitly call 'disconnect'.  Also,
garbage collection may not run when the program terminates, and some databases
really like an explicit disconnect.

So, bottom line is, you're best off calling 'disconnect' directly, but the
world won't end if you forget.

This function discards any data not committed already.  Database driver
implementators should explicitly call 'rollback' if their databases don't
do this automatically on disconnect.

Bad Things (TM) could happen if you call this while you have 'Statement's 
active.  In more precise language, the results in such situations are undefined
and vary by database.  So don't do it.

-}
                disconnect :: conn -> IO (SqlResult ())
                {- | Explicitly start new transaction -}
                start :: conn -> IO (SqlResult ())
                {- | Commit any pending data to the database.

                   Required to make any changes take effect. -}
                commit :: conn -> IO (SqlResult ())
                {- | Roll back to the state the database was in prior to the
                   last 'commit' or 'rollback'. -}
                rollback :: conn -> IO (SqlResult ())
                {- | Execute an SQL string, which may contain multiple
                   queries. This is intended for situations where you
                   need to run DML or DDL queries and aren't
                   interested in results. -}
                prepare :: conn -> String -> IO (SqlResult Statement)
                {- | Create a new 'Connection' object, pointed at the same
                   server as this object is.  This will generally establish
                   a separate physical connection.

                   When you wish to establish multiple connections to a single
                   server, the correct way to do so is to establish the
                   first connection with the driver-specific connection
                   function, and then clone it for each additional connection.
                   
                   This can be important when a database doesn't provide
                   much thread support itself, and the HDBC driver module
                   must serialize access to a particular database.

                   This can also be a handy utility function whenever you
                   need a separate connection to whatever database you are
                   connected to already. -}
                clone :: conn -> IO (SqlResult conn)


                {- | The name of the HDBC driver module for this connection.
                   Ideally would be the same as the database name portion
                   of the Cabal package name.  For instance, \"sqlite3\"
                   or \"odbc\".  This is the layer that is bound most
                   tightly to HDBC. -}
                hdbcDriverName :: conn -> String
                {- | The version of the C (or whatever) client library
                   that the HDBC driver module is bound to.  The meaning
                   of this is driver-specific.  For an ODBC or similar
                   proxying driver, this should be the version of the
                   ODBC library, not the eventual DB client driver. -}
                hdbcClientVer :: conn -> String
                {- | In the case of a system such as ODBC, the name of
                   the database client\/server in use, if available.
                   For others,
                   identical to 'hdbcDriverName'. -}
                proxiedClientName :: conn -> String
                {- | In the case of a system such as ODBC, the version of
                   the database client in use, if available.  For others,
                   identical to 'hdbcClientVer'. This is the next layer
                   out past the HDBC driver. -}
                proxiedClientVer :: conn -> String
                {- | The version of the database server, if available. -}
                dbServerVer :: conn -> String
                {- | Whether or not the current database supports transactions.
                   If False, then 'commit' and 'rollback' should be expected
                   to raise errors.

                   MySQL is the only commonly-used database that is known
                   to not support transactions entirely.  Please see
                   the MySQL notes in the ODBC driver for more information. -}
                dbTransactionSupport :: conn -> Bool


