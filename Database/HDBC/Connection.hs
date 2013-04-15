{-# LANGUAGE
    TypeFamilies
  , DeriveDataTypeable
  , ExistentialQuantification
  , FlexibleContexts
 #-}

{- |
   Module     : Database.HDBC.Types
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Types for HDBC.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Connection
       (
         -- * Typeclasses
         Connection(..)
         -- * Data types
       , ConnStatus(..)
       , ConnWrapper(..)
         -- * functions
       , castConnection
       ) where

import qualified Data.Text.Lazy as TL

import Database.HDBC.Statement
import Database.HDBC.SqlError

import Data.Typeable

-- | Connection status
data ConnStatus = ConnOK           -- ^ Successfully connected
                | ConnDisconnected -- ^ Successfully disconnected, all
                                   -- statements must be closed at this state
                | ConnBad          -- ^ Some bad situation

-- | Typeclass to abstract the working with connection.                  
class (Typeable conn, (Statement (ConnStatement conn))) => Connection conn where
  
  -- | Statement overloded type for specific connection type
  type ConnStatement conn :: *
  
  -- | Disconnection from the database, every opened statement must be finished
  -- after this method executed. 
  disconnect :: conn -> SqlResult ()

  -- | Explicitly start the transaction. Without starting the transaction you
  -- can not commit or rollback it. HDBC does not check if transaction started
  -- or not, this is the application's resposibility.
  --
  -- This is not recomended to use 'start' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  start :: conn -> SqlResult ()

  -- | Explicitly commit started transaction. You must 'start' the transaction
  -- before 'commit'
  --
  -- This is not recomended to use 'commit' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  commit :: conn -> SqlResult ()

  -- | Rollback the transaction's state. You must 'start' the transaction before
  -- 'rollback'
  --
  -- This is not recomended to use 'rollback' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  rollback :: conn -> SqlResult ()

  -- | Check if current connection is in transaction state. Return True if
  -- transaction is started. Each driver implements it with it's own way: some
  -- RDBMS has API to check transaction state (like PostgreSQL), some has no
  -- (like Sqlite3), so this drivers just save some flag 
  inTransaction :: conn -> SqlResult Bool

  connStatus :: conn -> SqlResult ConnStatus

  -- | Prepare the statement. Some databases has no feature of preparing
  -- statements (PostgreSQL can just prepare named statements), so each driver
  -- behaves it's own way.
  prepare :: conn -> TL.Text -> SqlResult (ConnStatement conn)

  -- | Clone the database connection. Return new connection with the same
  -- settings
  clone :: conn -> SqlResult conn

  -- | The name of the HDBC driver module for this connection. Ideally would be
  -- the same as the database name portion of the Cabal package name.  For
  -- instance, \"sqlite3\" or \"postgresql\".  This is the layer that is bound most
  -- tightly to HDBC
  hdbcDriverName :: conn -> String

  -- | The version of the C (or whatever) client library that the HDBC driver
  -- module is bound to.  The meaning of this is driver-specific.  For an ODBC
  -- or similar proxying driver, this should be the version of the ODBC library,
  -- not the eventual DB client driver.
  hdbcClientVer :: conn -> String

  -- | In the case of a system such as ODBC, the name of the database
  -- client\/server in use, if available. For others, identical to
  -- 'hdbcDriverName'.
  proxiedClientName :: conn -> String

  -- | In the case of a system such as ODBC, the version of the database client
  -- in use, if available.  For others, identical to 'hdbcClientVer'. This is
  -- the next layer out past the HDBC driver.
  proxiedClientVer :: conn -> String

  -- | The version of the database server, if available.
  dbServerVer :: conn -> String

  -- | Whether or not the current database supports transactions. If False, then
  -- 'commit' and 'rollback' should be expected to raise errors.
  dbTransactionSupport :: conn -> Bool


-- | Wrapps the specific connection. You can write database-independent code
-- mixing it with database-dependent using 'castConnection' function to cast
-- Wrapper to specific connection type, if you need.
data ConnWrapper = forall conn. Connection conn => ConnWrapper conn
                   deriving (Typeable)

instance Connection ConnWrapper where
  type ConnStatement ConnWrapper = StmtWrapper

  disconnect (ConnWrapper conn) = disconnect conn
  start (ConnWrapper conn) = start conn
  commit (ConnWrapper conn) = commit conn
  rollback (ConnWrapper conn) = rollback conn
  inTransaction (ConnWrapper conn) = inTransaction conn
  connStatus (ConnWrapper conn) = connStatus conn
  prepare (ConnWrapper conn) str = (prepare conn str) >>= (\s -> return $ StmtWrapper s)
  clone (ConnWrapper conn) = (clone conn) >>= (\c -> return $ ConnWrapper c)
  hdbcDriverName (ConnWrapper conn) = hdbcDriverName conn
  hdbcClientVer (ConnWrapper conn) = hdbcClientVer conn
  proxiedClientName (ConnWrapper conn) = proxiedClientName conn
  proxiedClientVer (ConnWrapper conn) = proxiedClientVer conn
  dbServerVer (ConnWrapper conn) = dbServerVer conn
  dbTransactionSupport (ConnWrapper conn) = dbTransactionSupport conn

-- | Cast wrapped connection to the specific connection type using 'cast' of
-- 'Typeable'. You can write database-specific code safely casting wrapped
-- connection to specific type dynamically.
castConnection :: (Connection conn) => ConnWrapper -> Maybe conn
castConnection (ConnWrapper conn) = cast conn
  
