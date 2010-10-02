{-# Language PatternGuards #-}

------------------------------------------------------------------
-- |
-- Module      :  System.IO9.NameSpaceT
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- NameSpace Layer and Monad Transformer
------------------------------------------------------------------

module System.IO9.NameSpaceT (
  BindFlag (..)
 ,NameSpaceT
 ,showNS
 ,bindPath
) where

import GHC.IO (catchException)
import Data.Char
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import System.FilePath
import System.IO9.DevLayer
import System.IO9.Error
import Control.Exception
import qualified Data.DList as DL
import qualified Data.Map as M

-- Need this to keep compiler happy.

instance (Show a) => Show (DL.DList a) where
  show x = show (DL.toList x)

instance (Ord a) => Ord (DL.DList a) where
  compare x y = compare (DL.toList x) (DL.toList y)

instance (Eq a) => Eq (DL.DList a) where
  x == y = (DL.toList x) == (DL.toList y)

-- A datatype to represent a component of a bound directory. Lists of bound directories
-- are stored in the namespace map as values.

data BoundDir = BoundDir {
  dirfp :: FilePath                -- ^ Absolute path to the actual directory
 ,dirfl :: BindFlag                -- ^ Copy of the bind flags as they were supplied
 ,dircr :: Bool                    -- ^ 'True' if creation of files is allowed
} deriving (Eq, Ord, Show)

-- | A datatype to represent binding mode: before/after/replace, and
-- the boolean flag to allow/prohibit creation of files in this actual directory
-- (the latter has nothing to do with actual directory permissions: it only affects
-- which directory in the union is chosen for creation of a new file).

data BindFlag = BindBefore Bool    -- ^ Bind before any directory bound to this point
              | BindAfter Bool     -- ^ Bind after any directory bound to this point
              | BindRepl           -- ^ Replace all bindings at this point with this directory
                deriving (Eq, Ord, Show)

-- | A datatype to hold bound directories in an ordered list.

newtype UnionDir = UnionDir {unDir :: DL.DList BoundDir} deriving (Eq, Ord, Show)

-- | Create an union directory containing a single directory. file creation
-- is enabled by default.

unionDir :: FilePath -> UnionDir
unionDir fp = UnionDir (DL.singleton BoundDir {dirfp = fp, dirfl = BindRepl, dircr = True})

-- | Bind an actual directory at the given union point.

addUnion  :: UnionDir              -- ^ The union point where to bind a directory
          -> FilePath              -- ^ Actual file path to bind (not checked for existence)
          -> BindFlag              -- ^ Bind mode
          -> UnionDir              -- ^ Updated union point.
addUnion (UnionDir dl) fp bf = case bf of
  BindRepl -> unionDir fp
  BindBefore cr -> UnionDir $ DL.cons (BoundDir {dirfp = fp, dirfl = bf, dircr = cr}) dl
  BindAfter cr -> UnionDir $ DL.snoc dl (BoundDir {dirfp = fp, dirfl = bf, dircr = cr})

-- Namespace is a map where evaluable file paths are keys, and evaluated file paths along with
-- union points are values.

data UnionPoint = UnionPoint UnionDir FilePath

type NameSpace = M.Map FilePath UnionPoint

-- | Show a namespace as a sequence of Plan9 bind (1) commands.

showNS :: (MonadIO m) => NameSpaceT m [String]

showNS = NameSpaceT $ do
  asks nspace >>= liftIO . readMVar >>= return . concatMap showUP . M.toList where
    showUP (fp, (UnionPoint (UnionDir dl) _)) = map onebind (DL.toList dl) where
      onebind bd = "bind " ++ flgb (dirfl bd) ++ flgc (dircr bd)  ++ dirfp bd ++ fp
      flgb (BindBefore _) = "-b "
      flgb (BindAfter _) = "-a "
      flgb  BindRepl = ""
      flgc  True = "-c "
      flgc  False = ""
  

-- Namespace execution environment consists of the kernel devices table
-- (immutable), and a namespace itself (mutable transactional variable).

data NsEnv = NsEnv {
   kdtbl :: M.Map Char DevTable
  ,nspace :: MVar NameSpace
}

-- | A monad transformer over an IO-capable monad to provide namespaced I/O API.

newtype (MonadIO m) => NameSpaceT m a = NameSpaceT {runNameSpaceT :: ReaderT NsEnv m a}

instance (MonadIO m) => Monad (NameSpaceT m) where
    return = NameSpaceT . return
    m >>= k = NameSpaceT $ runNameSpaceT . k =<< runNameSpaceT m
    fail msg = NameSpaceT $ fail msg

-- | Bind a path somewhere in the namespace. Both paths should be absolute or device, and will be 
-- evaluated. One exception however applies when binding to the "/" old path to the empty 
-- namespace, evaluation does not occur provided that the new path is a device path.
-- If any of paths is neither absolute nor device, failure occurs.

bindPath :: MonadIO m
         => BindFlag                   -- ^ Bind options (before, after, create etc.)
         -> FilePath                   -- ^ New path
         -> FilePath                   -- ^ Old path
         -> NameSpaceT m ()            -- ^ No return value, namespace updated under the hood

bindPath _ new old | not ((isAbsolute new || isDevice new) && (isAbsolute old || isDevice old)) =
  NameSpaceT $ liftIO $ throwIO Efilename


bindPath fl new old | old == "/" && isDevice new = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  liftIO $ withNameSpace mv $ \ns -> case M.null ns of
    True -> do
      let ud = unionDir new
      return $ M.insert old (UnionPoint ud new) ns
    False -> bind_common fl new old dtb ns

bindPath fl new old = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  liftIO $ withNameSpace mv $ bind_common fl new old dtb

bind_common :: BindFlag -> FilePath -> FilePath -> M.Map Char DevTable -> NameSpace -> IO NameSpace

bind_common _ _ _ _ _ = fail "not implemented"


-- Do something with a NameSpace, taking care of exceptions.
-- A current NameSpace is referred to by a MVar, and thus can be shared among several
-- threads. Usual takeMVar/putMVar mechanism provides serialized access to the NameSpace.
-- Here, the logic similar to one used with Handles, is employed: if during an operation
-- over a NameSpace an exception happens, the handler will restore the MVar contents.

withNameSpace :: MVar NameSpace -> (NameSpace -> IO NameSpace) -> IO ()

withNameSpace mv f = do
  ns' <- do_operation mv f
  putMVar mv ns'
  return ()

do_operation mv f = do
  ns <- takeMVar mv
  f ns `catchException` handler ns
  where
    handler ns e = do
      putMVar mv ns
      case () of
        _ | Just async_ex <- fromException e -> do
            let _ = async_ex :: AsyncException
            t <- myThreadId
            throwTo t e
            do_operation mv f
        _otherwise ->
            throwIO e

-- | Return 'True' is the given path is a device path (starts with #).

isDevice :: FilePath -> Bool

isDevice ('#':_) = True
isDevice _ = False

-- | Extract the device letter (if any) from the path. If this is not a device path,
-- the 0 character is returned.

deviceOf :: FilePath -> Char

deviceOf ('#':d:_) = d
deviceOf _ = chr 0

-- | Extract the device file tree (if any) from the path. If this is not a device path
-- an empty string is returned.

treeOf :: FilePath -> FilePath

treeOf fp@('#':_) = let (('#':_:tree):_) = splitPath fp in tree
treeOf _ = ""


