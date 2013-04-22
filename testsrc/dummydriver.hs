{-# LANGUAGE
  DeriveDataTypeable
, TypeFamilies
, OverloadedStrings
, ScopedTypeVariables
  #-}

module Main where

-- import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Hspec.Expectations

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Typeable

import qualified Data.Text.Lazy as TL

import Database.HDBC
import Database.HDBC.DriverUtils

data TStatus = TIdle | TInTransaction
             deriving (Eq, Show, Read, Typeable)

data DummyConnection =
  DummyConnection { dcState :: MVar ConnStatus
                  , dcTrans :: MVar TStatus
                  , dcChilds :: ChildList DummyStatement
                  , dcTransSupport :: Bool
                  }
  deriving (Typeable, Eq)

data DummyStatement =
  DummyStatement { dsConnection :: DummyConnection
                 , dsQuery :: TL.Text
                 , dsStatus :: MVar StatementStatus
                 }
  deriving (Typeable, Eq)
  

newConnection transSupport = DummyConnection
                             <$> newMVar ConnOK
                             <*> newMVar TIdle
                             <*> newChildList
                             <*> return transSupport

withOKConnection :: DummyConnection -> IO a -> IO a
withOKConnection conn action = do
  st <- connStatus conn
  case st of
    ConnOK -> action
    _ -> throwIO $ SqlError 1 $ "Connection has wrong status " ++ show st

withTransactionSupport :: DummyConnection -> IO a -> IO a
withTransactionSupport conn action = case (dcTransSupport conn) of
  True -> action
  False -> throwIO $ SqlError 10 "Transaction is not supported by this connection"
  

instance Connection DummyConnection where
  type ConnStatement DummyConnection = DummyStatement
  disconnect conn = modifyMVar_ (dcState conn) $ const $ return ConnDisconnected
  start conn = withOKConnection conn
               $ withTransactionSupport conn
               $ modifyMVar_ (dcTrans conn)
               $ \s -> case s of
    TIdle -> return TInTransaction
    TInTransaction -> throwIO $ SqlError 2 $ "Connection is already in transaction "
  commit conn = withOKConnection conn
                $ withTransactionSupport conn
                $ modifyMVar_ (dcTrans conn)
                $ \s -> case s of
    TInTransaction -> return TIdle
    TIdle -> throwIO $ SqlError 3 $ "Connection is not in transaction to commit"
  rollback conn = withOKConnection conn
                  $ withTransactionSupport conn
                  $ modifyMVar_ (dcTrans conn)
                  $ \s -> case s of
    TInTransaction -> return TIdle
    TIdle -> throwIO $ SqlError 4 $ "Connection is not in transaction to rollback"
  inTransaction conn = withTransactionSupport conn $ do
    t <- takeMVar $ dcTrans conn
    return $ t == TInTransaction
  connStatus = takeMVar . dcState
  prepare conn query = do
    st <- DummyStatement
          <$> return conn
          <*> return query
          <*> newMVar StatementNew
    -- addChild (dcChilds conn) st
    return st
  clone conn = DummyConnection
               <$> (newMVar ConnOK)
               <*> (newMVar TIdle)
               <*> newChildList
               <*> (return $ dcTransSupport conn)
  hdbcDriverName = const "DummyDriver"
  hdbcClientVer = const "0"
  proxiedClientName = const "DummyDriver"
  proxiedClientVer = const "0"
  dbServerVer = const "0"
  dbTransactionSupport = dcTransSupport

  
instance Statement DummyStatement where
  execute stmt _ = modifyMVar_ (dsStatus stmt) $ \st -> do
    case st of
      StatementNew -> do
        if (originalQuery stmt) == "throw"
          then throwIO $ SqlError 5 "Throwed query exception"
          else return StatementExecuted
      _ -> throwIO $ SqlError 6 $ "Statement has wrong status to execute query " ++ show st
    
  statementStatus = takeMVar . dsStatus

  affectedRows = const $ return 0
  finish stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementFinished
  reset stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementNew
  fetchRow = const $ return Nothing
  getColumnNames = const $ return []
  originalQuery = dsQuery



test1 = do
  c <- newConnection True
  (withTransaction c $ do
      intr <- inTransaction c
      intr `shouldBe` True
      stmt <- prepare c "throw"
      print "fuck"
      execute stmt []
    ) `shouldThrow` (\(_ :: SqlError) -> True)
  intr <- inTransaction c
  intr `shouldBe` False 



  
main :: IO ()
main = defaultMain [
  testCase  "Transaction handling" test1
  ]
