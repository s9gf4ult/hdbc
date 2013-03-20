{-# LANGUAGE
    TypeFamilies #-}

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
       ) where

import Database.HDBC.Statement (Statement)
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
