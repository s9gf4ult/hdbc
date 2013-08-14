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
  
  ChildList
  , closeAllChildren
  , addChild
  , newChildList
  )

where
import Control.Concurrent.MVar
import System.Mem.Weak
import Control.Monad
import Database.HDBI.Types (Statement(..))

-- | List of weak pointers to childs with concurrent access
type ChildList stmt = MVar [Weak stmt]


-- | new empty child list
newChildList :: IO (ChildList stmt)
newChildList = newMVar []

{- | Close all children.  Intended to be called by the 'disconnect' function
in 'Connection'. 

There may be a potential race condition wherein a call to newSth at the same
time as a call to this function may result in the new child not being closed.
-}
closeAllChildren :: (Statement stmt) => (ChildList stmt) -> IO ()
closeAllChildren mcl = modifyMVar_ mcl $ \ls -> do
  mapM_ closefunc ls
  return ls
    where closefunc child =
              do c <- deRefWeak child
                 case c of
                   Nothing -> return ()
                   Just x -> finish x

{- | Adds a new child to the existing list.  Also takes care of registering
a finalizer for it, to remove it from the list when possible. -}
addChild :: (Statement stmt) => (ChildList stmt) -> stmt -> IO ()
addChild mcl stmt = 
    do weakptr <- mkWeakPtr stmt (Just (childFinalizer stmt mcl))
       modifyMVar_ mcl (\l -> return (weakptr : l))

{- | The general finalizer for a child.

It is simply a filter that removes any finalized weak pointers from the parent.

If the MVar is locked at the start, does nothing to avoid deadlock.  Future
runs would probably catch it anyway. -}
childFinalizer :: (Statement stmt) => stmt -> ChildList stmt -> IO ()
childFinalizer stmt mcl = do
  finish stmt                   -- make sure the statement is finished
  c <- isEmptyMVar mcl
  case c of
    True   -> return ()
    False  -> modifyMVar_ mcl (filterM filterfunc)
    
  where filterfunc c = do
          dc <- deRefWeak c
          case dc of
            Nothing -> return False
            Just _ -> return True
