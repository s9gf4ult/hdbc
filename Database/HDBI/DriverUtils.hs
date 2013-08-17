{- |
   Module     : Database.HDBI.DriverUtils
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : BSD3

   Maintainer : Aleksey Uymanov <s9gf4ult@gmail.com>
   Stability  : experimental
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBI.DriverUtils (
-- | Utilities for database backend drivers.
--
-- Please note: this module is intended for authors of database driver libraries
-- only.  Authors of applications using HDBI should use 'Database.HDBI'
-- exclusively.

  ChildList(..)
  , closeAllChildren
  , addChild
  , newChildList
  )

where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Database.HDBI.Types (Statement(..))
import System.Mem.Weak

-- | List of weak pointers to childs with concurrent access
data ChildList stmt = ChildList
                      { clList ::  MVar [Weak stmt]
                      , clCounter :: TVar Int -- ^ Little hackish child counter,
                                             -- need to wait all child
                                             -- finalizers in 'closeAllChildren'
                      }


-- | new empty child list
newChildList :: IO (ChildList stmt)
newChildList = ChildList
               <$> newMVar []
               <*> newTVarIO 0

{- | Close all children.  Intended to be called by the 'disconnect' function
in 'Connection'.

There may be a potential race condition wherein a call to newSth at the same
time as a call to this function may result in the new child not being closed.
-}
closeAllChildren :: (Statement stmt) => (ChildList stmt) -> IO ()
closeAllChildren mcl = do
  withMVar (clList mcl) $ mapM_ finalize
  atomically $ do               -- wait until counter becomes 0. In other words,
                                -- wait until all finalizers run
    a <- readTVar $ clCounter mcl
    when (a > 0) retry

  return ()
    where

{- | Adds a new child to the existing list.  Also takes care of registering
a finalizer for it, to remove it from the list when possible. -}
addChild :: (Statement stmt) => (ChildList stmt) -> stmt -> IO ()
addChild mcl stmt = modifyMVar_ (clList mcl) $ \l -> do
  atomically $ modifyTVar' (clCounter mcl) (+1)
  weakptr <- mkWeakPtr stmt (Just (childFinalizer stmt mcl))
  return $ weakptr:l

{- | The general finalizer for a child.

It is simply a filter that removes any finalized weak pointers from the parent.

If the MVar is locked at the start, does nothing to avoid deadlock.  Future
runs would probably catch it anyway. -}
childFinalizer :: (Statement stmt) => stmt -> ChildList stmt -> IO ()
childFinalizer stmt mcl = do
  finish stmt                   -- make sure the statement is finished
  atomically $ modifyTVar' (clCounter mcl) (\x -> x - 1)
  c <- isEmptyMVar $ clList mcl  -- ignore if MVar is already taken by someone
  case c of
    True   -> return ()
    False  -> modifyMVar_ (clList mcl) (filterM filterfunc)

  where
    filterfunc c = do
      dc <- deRefWeak c
      case dc of
        Nothing -> return False
        Just _ -> return True
