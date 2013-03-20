{-# LANGUAGE
    TypeFamilies
  , DeriveDataTypeable #-}

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
         Connection(..)
       , ConnStatus(..)
       , ConnWrapper(..)
       ) where

import Database.HDBC.Statement
import Database.HDBC.SqlError

import Data.Typeable

data ConnStatus = ConnOK
                | ConnDisconnected
                | ConnBad

class (Typeable conn, (Statement (ConnStatement conn))) => Connection conn where
  type ConnStatement conn :: *

  disconnect :: conn -> SqlResult ()
  start :: conn -> SqlResult ()
  commit :: conn -> SqlResult ()
  rollback :: conn -> SqlResult ()
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
