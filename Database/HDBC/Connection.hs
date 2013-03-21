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
         Connection(..)
       , ConnStatus(..)
       , ConnWrapper(..)
       ) where

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
  
  -- | Statement overloded type for concrete connection
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
  prepare :: conn -> String -> SqlResult (ConnStatement conn)
  clone :: conn -> SqlResult conn
  hdbcDriverName :: conn -> String
  hdbcClientVer :: conn -> String
  proxiedClientName :: conn -> String
  proxiedClientVer :: conn -> String
  dbServerVer :: conn -> String
  dbTransactionSupport :: conn -> Bool

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
