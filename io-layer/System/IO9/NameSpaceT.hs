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
 ,initNS
 ,showNS
 ,dbgPrint
 ,bindPath
 ,EvalPath
 ,evalPath
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

type DevMap = M.Map Char DevTable

-- | Show a namespace as a sequence of Plan9 bind (1) commands.

showNS :: (MonadIO m) => NameSpaceT m [String]

showNS = NameSpaceT $ do
  asks nspace >>= liftIO . readMVar >>= return . concatMap showUP . M.toList where
    showUP (fp, (UnionPoint (UnionDir dl) _)) = map onebind (DL.toList dl) where
      onebind bd = "bind " ++ flgb (dirfl bd) ++ flgc (dircr bd)  ++ dirfp bd ++ " " ++ fp
      flgb (BindBefore _) = "-b "
      flgb (BindAfter _) = "-a "
      flgb  BindRepl = ""
      flgc  True = "-c "
      flgc  False = ""
  
-- | Run the "init" program with the given device list and empty namespace
-- (it is expected that it builds the namespace from scratch).

initNS :: (MonadIO m) => [DevTable] -> NameSpaceT m () -> m ()

initNS dts nsi = do
  mv <- liftIO $ newMVar (M.empty)
  let dvm = M.fromList $ zip (map devchar dts) dts
      env = NsEnv {
        kdtbl = dvm
       ,nspace = mv
      }
  runNameSpaceT nsi `runReaderT` env

-- Namespace execution environment consists of the kernel devices table
-- (immutable), and a namespace itself (mutable transactional variable).

data NsEnv = NsEnv {
   kdtbl :: DevMap
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

bind_common fl new old dtb ns = do
  let evalpath fp = eval_common fp `runReaderT` (dtb, ns)
  epnew <- evalpath new
  epold <- evalpath old
  let norm = normalise $ epCanon epold ++ "/"
      oldpath = show $ epAttach epold
      newpath = show $ epAttach epnew
      uds = unionDir oldpath
      udx = addUnion uds newpath fl
      up = UnionPoint udx oldpath
      modx _ (UnionPoint ud fp) = UnionPoint (addUnion ud newpath fl) fp
      ns' = M.insertWith modx norm up ns
  return ns

-- | A data type to represent an evaluated path.

data EvalPath = EvalPath {
  epAttach :: DevAttach                 -- ^ Attachment desctiptor for the path if evaluated
 ,epCanon :: FilePath                   -- ^ Canonicalized (with dot-elements removed) path
} deriving (Show)

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, an attachment descriptor for the path is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

evalPath :: (MonadIO m) => FilePath -> NameSpaceT m EvalPath

evalPath fp | not (isAbsolute fp || isDevice fp) =
  NameSpaceT $ liftIO $ throw Efilename

evalPath fp = NameSpaceT $ do
  ns <- asks nspace >>= liftIO . readMVar
  kd <- asks kdtbl
  liftIO $ eval_common fp `runReaderT` (kd, ns)

-- Common function for path evaluation use by both bindPath and evalPath.
-- It operates on the immutable copy of the namespace, thus it uses a separate
-- reader transformer.

type EvalM = ReaderT (DevMap, NameSpace) IO

eval_common :: FilePath -> EvalM EvalPath

eval_common fp = eval_root (splitPath fp) []

-- Root element. Look it up in the namespace. If not found, fail. If found,
-- place it on the evaluated path, remove from the unevaluated part, recurse. 
-- History must be empty at this point.

eval_root :: [FilePath] -> [FilePath] -> EvalM EvalPath

eval_root ("/" : rawps) _ = do
  rootd <- asks snd >>= findroot
  eval_root (rootd : rawps) ["/"]

eval_root _ _ = liftIO $ throwIO Efilename


-- Find the root entry in the namespace. The root entry is special that it always has
-- one directory bound. So, only a single entry is returned (in the case of multiple
-- directories unioned under the root entry, head of the list is returned). If no
-- root entry found, fail.

findroot :: NameSpace -> EvalM FilePath

findroot ns = do
  fps <- findunion "/" ns
  case fps of
    (fp:_) | isDevice fp -> return fp
    _ -> liftIO $ throwIO $ OtherError "eval: no root binding in the namespace"

-- Find all files/directories bound at the given union point. If the namespace provided
-- does not contain the union point provided, return an empty list.
-- No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> EvalM [FilePath]

findunion fp ns = do
  let fp' = if fp == "/" then fp else normalise (fp ++ "/")
      up = M.lookup fp' ns
  case up of
    Just (UnionPoint ud _) -> return . map dirfp . DL.toList $ unDir ud 
    _ -> return []



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


dbgPrint :: MonadIO m => String -> NameSpaceT m ()

dbgPrint s = NameSpaceT $ liftIO $ putStrLn s

