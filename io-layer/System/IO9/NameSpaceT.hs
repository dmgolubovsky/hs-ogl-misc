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
 ,nsInit
 ,dbgPrint
 ,PathHandle (phCanon)
 ,FileHandle
 ,nsBind
 ,nsEval
 ,nsCreate
 ,nsRemove
 ,nsStat
 ,nsWstat
) where

import GHC.IO (catchException)
import GHC.IO.Handle
import Data.Bits
import Data.Char
import Data.Word
import Data.List
import Data.NineP
import Data.NineP.Bits
import Control.Monad
import Control.Monad.Error
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
  dirph :: PathHandle              -- ^ Actual directory object information
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

unionDir :: PathHandle -> UnionDir
unionDir ph = UnionDir (DL.singleton BoundDir {dirph = ph, dirfl = BindRepl, dircr = True})

-- | Bind an actual directory at the given union point.

addUnion  :: UnionDir              -- ^ The union point where to bind a directory
          -> PathHandle            -- ^ Actual file path to bind (not checked for existence)
          -> BindFlag              -- ^ Bind mode
          -> UnionDir              -- ^ Updated union point.
addUnion (UnionDir dl) ph bf = case bf of
  BindRepl -> unionDir ph
  BindBefore cr -> UnionDir $ DL.cons (BoundDir {dirph = ph, dirfl = bf, dircr = cr}) dl
  BindAfter cr -> UnionDir $ DL.snoc dl (BoundDir {dirph = ph, dirfl = bf, dircr = cr})

-- Namespace is a map where evaluable file paths are keys, and evaluated file paths along with
-- union points are values.

data UnionPoint = UnionPoint UnionDir FilePath deriving (Show)

type NameSpace = M.Map FilePath UnionPoint

type DevMap = M.Map Char DevTable

-- | Run the "init" program with the given device list and empty namespace
-- (it is expected that it builds the namespace from scratch).

nsInit :: (MonadIO m) => [DevTable] -> NameSpaceT m () -> m ()

nsInit dts nsi = do
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

bind_common :: BindFlag -> FilePath -> FilePath -> M.Map Char DevTable -> NameSpace -> IO NameSpace

bind_common fl new old dtb ns = do
  let evalpath fp = eval_common fp `runReaderT` (dtb, ns)
  epnew <- evalpath new
  epold <- evalpath old
  let norm = normalise $ phCanon epold ++ "/"
      oldpath = show $ phAttach epold
      newpath = show $ phAttach epnew
      uds = unionDir epold
      udx = addUnion uds epnew fl
      up = UnionPoint udx oldpath
      modx _ (UnionPoint ud fp) = UnionPoint (addUnion ud epnew fl) fp
      ns' = M.insertWith modx norm up ns
  return ns'

-- | A semi-opaque data type to represent an evaluated path. Note that path handles
-- identify filesystem objects by their name (paths), so if a file or directory
-- gets renamed behind the scenes, 'PathHandle's associated with them may become
-- invalid (unless the attachment descriptor is passed to the file server,
-- and the server has some way to track its objects by the embedded 'Qid').

data PathHandle = PathHandle {
  phAttach :: DevAttach                 -- ^ Attachment desctiptor for the path if evaluated
 ,phCanon :: FilePath                   -- ^ Canonicalized (with dot-elements removed) path
} deriving (Show)

instance Eq PathHandle where
  p1 == p2 = phCanon p1 == phCanon p2

instance Ord PathHandle where
  compare p1 p2 = compare (phCanon p1) (phCanon p2)

-- | An opaque data structure to represent an open file or a directory. For a file,
-- this is just a wrapper for a regular GHC handle; for a directory, this is a list
-- of directory handles belonging to an union, if any (or a singleton handle list
-- for non-union directories).

data FileHandle = 
   FileHandle Handle                   -- ^ Wraps a regular file handle
 | DirHandle [(DevAttach, Handle)]     -- ^ Wraps several directory handles

-- | Bind a path somewhere in the namespace. Both paths should be absolute or device, and will be 
-- evaluated. One exception however applies when binding to the "/" old path to the empty 
-- namespace, evaluation does not occur provided that the new path is a device path.
-- If any of paths is neither absolute nor device, failure occurs.

nsBind :: MonadIO m
       => BindFlag                     -- ^ Bind options (before, after, create etc.)
       -> FilePath                     -- ^ New path
       -> FilePath                     -- ^ Old path
       -> NameSpaceT m ()              -- ^ No return value, namespace updated under the hood

nsBind _ new old | not ((isAbsolute new || isDevice new) && (isAbsolute old || isDevice old)) =
  NameSpaceT $ liftIO $ throwIO Efilename


nsBind fl new old | old == "/" && isDevice new = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  liftIO $ withNameSpace mv $ \ns -> case M.null ns of
    True -> do
      let newnorm = normalise new
      attnew <- attdev newnorm `runReaderT` (dtb, ns)
      let phnew = PathHandle {phAttach = attnew, phCanon = newnorm}
          ud = unionDir phnew
      return $ M.insert old (UnionPoint ud new) ns
    False -> bind_common fl new old dtb ns

nsBind fl new old = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  liftIO $ withNameSpace mv $ bind_common fl new old dtb

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, an attachment descriptor for the path is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

nsEval :: (MonadIO m) => FilePath -> NameSpaceT m PathHandle

nsEval fp | not (isAbsolute fp || isDevice fp) =
  NameSpaceT $ liftIO $ throw Efilename

nsEval fp = NameSpaceT $ do
  ns <- asks nspace >>= liftIO . readMVar
  kd <- asks kdtbl
  liftIO $ eval_common fp `runReaderT` (kd, ns)

-- | Create a new file or directory (set 'c_DMDIR' in the @mode@ argument).
-- Creation in an union directory follows the Plan9 semantics by finding the
-- first member of the union that allows creation. The 'FilePath' supplied should not
-- contain slashes, otherwise an error will be thrown.

nsCreate :: MonadIO m
         => PathHandle                    -- ^ Handle of the directory
         -> FilePath                      -- ^ Name of the file or directory to create
         -> Word32                        -- ^ Creation mode/permissions
         -> NameSpaceT m PathHandle       -- ^ Handle of the created object

nsCreate dph fp mode | '/' `elem` fp = NameSpaceT $ liftIO $ throwIO Ebadarg

nsCreate dph fp mode = NameSpaceT $ do
  when (((qid_typ $ devqid $ phAttach dph) .&. c_QTDIR) == 0) $ liftIO $ throwIO Enotdir
  ns <- asks nspace >>= liftIO . readMVar
  let un = findunion (phCanon dph) ns
      dirs = filter dircr un
      dda = case dirs of
        [] -> phAttach dph
        (d:_) -> phAttach $ dirph d
  when (null dirs && not (null un)) $ liftIO $ throwIO Enocreate
  da <- liftIO $ devCreate dda fp mode
  let newpath = tail $ normalise ("x" ++ phCanon dph ++ "/" ++ fp)
  return PathHandle {
                phCanon = newpath
               ,phAttach = da}
  

-- | Remove a file or a directory whose 'PathHandle' is provided. Fails if a non-empty
-- directory is to be removed.

nsRemove :: MonadIO m
         => PathHandle                    -- ^ Handle of the object to be removed
         -> NameSpaceT m ()               -- ^ Nothing is returned

nsRemove ph = NameSpaceT $ liftIO $ devRemove $ phAttach ph

-- | Obtain attributes of a file or directory. Note that for directories,
-- attributes of their server objects will be returned rather than of anything
-- unioned with them.

nsStat :: MonadIO m
       => PathHandle                      -- ^ Handle of the object whose attributes are requested
       -> NameSpaceT m Stat               -- ^ Result

nsStat ph = NameSpaceT $ liftIO $ devStat (phAttach ph)

-- | Change some attributes of a file or directory. See <http://man.cat-v.org/plan_9/5/stat>.
-- If the 'st_name' member of the provided 'Stat' structure contains a slash, error
-- will be thrown.

nsWstat :: (MonadIO m)
        => PathHandle                     -- ^ Handle of the object whose attributes to change
        -> Stat                           -- ^ A 'Stat' structure whose fields specify changes
        -> NameSpaceT m PathHandle        -- ^ Handle of the same object with updated attrs.

nsWstat ph st | '/' `elem` st_name st = NameSpaceT $ liftIO $ throwIO Ebadarg

nsWstat ph st = NameSpaceT $ liftIO $ do
  nda <- devWstat (phAttach ph) st
  nst <- devStat nda
  let ncn = replaceFileName (phCanon ph) (st_name nst)
  return PathHandle {
                phCanon = ncn
               ,phAttach = nda}
  

-- Common function for path evaluation use by both nsBind and nsEval.
-- It operates on the immutable copy of the namespace, thus it uses a separate
-- reader transformer.

type EvalM = ReaderT (DevMap, NameSpace) IO

eval_common :: FilePath -> EvalM PathHandle

eval_common fp = eval_root (splitPath fp) []

-- Root element. Look it up in the namespace. If not found, fail. If found,
-- place it on the evaluated path, remove from the unevaluated part, recurse. 
-- History must be empty at this point.

eval_root :: [FilePath] -> [FilePath] -> EvalM PathHandle

eval_root ("/" : rawps) _ = do
  rootd <- asks snd >>= findroot
  eval_root (rootd : rawps) ["/"]

-- If the path to evaluate is indeed a device path, attach the given device and tree
-- and proceed as if it was the root element.

eval_root (dvp : rawps) xps | isDevice dvp = do
  da <- attdev dvp
  let orig = head (xps ++ [dvp])
  eval_step da rawps [orig] [dvp]

eval_root _ _ = liftIO $ throwIO Efilename

-- Evaluate the rest of the path step-wise.

eval_step :: DevAttach -> [FilePath] -> [FilePath] -> [FilePath] -> EvalM PathHandle

-- Entire raw path consumed: evaluation complete.

eval_step da [] orig eval = return PathHandle {
  phAttach = da
 ,phCanon = joinPath orig}

-- Dot on the path: skip it.

eval_step da ("./" : rawps) orig eval = eval_step da rawps orig eval
eval_step da ("." : rawps) orig eval = eval_step da rawps orig eval

-- DotDot: use the logic from http://doc.cat-v.org/plan_9/4th_edition/papers/lexnames
-- If original path consists of only one element, ignore dotdot.
-- If evaluated path consists of only one element, fail: this is an internal error.
-- Take one element off the original path. Lookup the namespace if the resulting original
-- path marks any of the union points. If it does, retrieve its evaluated counterpart:
-- parent may reside on a different filesystem than child. Substitute the retrieved evaluated
-- path to the evaluated path and do the next step.
-- Otherwise just the original path minus one element and evaluated path minus one element.
-- In either case, new FID has to be obtained fo the new evaluated path.

eval_step da ("../" : rawps) orig eval = eval_step da (".." : rawps) orig eval

eval_step da (".." : rawps) [orig] eval = eval_step da rawps [orig] eval

eval_step da (".." : rawps) orig [eval] = do
  throw $ OtherError $ "internal error: .. with singleton evaluated path at " ++ joinPath orig

eval_step da (".." : rawps) orig eval = do
  let chop = reverse . tail . reverse                     -- chop the last element off
      origp = chop orig
      evalp = chop eval
  up <- asks snd >>= return . M.lookup (joinPath origp)
  let neval = case up of
        Just (UnionPoint _ fp) -> splitPath fp
        _ -> evalp
  nda <- attdev (head neval)
  wda <- liftIO $ devWalk nda (joinPath $ tail neval)
  eval_step wda rawps origp neval

-- General case. Look up the already evaluated path in the namespace (resulting in
-- either single or multiple path). One of them may be the one we already have a DevAttach for
-- (that is, same as eval). Try to walk all of these paths to the next element of the
-- path to be evaluated until successful (if not, this is an error). Once the right path 
-- has been found, replace eval with it, and repeat until the path to evaluate is entirely
-- consumed.

eval_step da (rawp : rawps) orig eval = do
  let jeval = joinPath eval
      jorig = joinPath orig
      nrawp = normalise (rawp ++ if null rawps then "" else "/")
  undirs <- asks snd >>= return . findunion jorig >>=  return . map dirph >>=
    \ps -> return (if null ps then [da] else map phAttach ps)
  foldr mplus (throw Enonexist) $ flip map undirs $ \dda -> do
    wda <- liftIO $ (devWalk dda nrawp `catchException` (\(e :: NineError) -> fail ""))
    eval_step wda rawps (orig ++ [rawp]) (eval ++ [rawp])


 

-- Find the root entry in the namespace. The root entry is special that it always has
-- one directory bound. So, only a single entry is returned (in the case of multiple
-- directories unioned under the root entry, head of the list is returned). If no
-- root entry found, fail.

findroot :: NameSpace -> EvalM FilePath

findroot ns = do
  let fps = map dirph $ findunion "/" ns
  case fps of
    (PathHandle {phCanon = fp}:_) | isDevice fp -> return fp
    _ -> liftIO $ throwIO $ OtherError "eval: no root binding in the namespace"

-- Find all files/directories bound at the given union point. If the namespace provided
-- does not contain the union point provided, return an empty list.
-- No normalization or canonicalization of the union point path is done here,
-- as well as of the union point contents.

findunion :: FilePath -> NameSpace -> [BoundDir]

findunion fp ns = 
  let fp' = if fp == "/" then fp else normalise (fp ++ "/")
      up = M.lookup fp' ns
  in  case up of
        Just (UnionPoint ud _) -> DL.toList $ unDir ud 
        _ -> []



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

-- Attach a device using the given path (must be a device path)

attdev :: FilePath -> EvalM DevAttach

attdev fp | not (isDevice fp) = liftIO $ throwIO Ebadsharp

attdev fp = do
  kt <- asks fst
  let mbdt = M.lookup (deviceOf fp) kt
  liftIO $ case mbdt of
    Nothing -> throwIO Ebadsharp
    Just dt -> devAttach dt (treeOf fp)
  

-- | Extract the device letter (if any) from the path. If this is not a device path,
-- the 0 character is returned.

deviceOf :: FilePath -> Char

deviceOf ('#':d:_) = d
deviceOf _ = chr 0

-- | Extract the device file tree (if any) from the path. If this is not a device path
-- an empty string is returned.

treeOf :: FilePath -> FilePath

treeOf fp@('#':_) = let (('#':_:tree):_) = splitPath fp in case tree of
  "" -> "/"
  _ -> tree
treeOf _ = ""


dbgPrint :: MonadIO m => String -> NameSpaceT m ()

dbgPrint s = NameSpaceT $ liftIO $ putStrLn s


