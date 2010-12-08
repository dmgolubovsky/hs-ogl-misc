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
 ,nsFork
 ,nsWait
 ,dbgPrint
 ,dbgChunks
 ,PathHandle (phCanon)
 ,nsBind
 ,nsEval
 ,nsCreate
 ,nsRemove
 ,nsStat
 ,nsWstat
 ,nsWithText
 ,nsWithBin
 ,nsEnumText
 ,nsEnumBin
 ,nsEnumDir
 ,nsCatch
 ,nsFinally
 ,nsStdIn
 ,nsStdOut
 ,Application (..)
 ,AppTable (..)
 ,AppHandle
 ,appTable
 ,appEntry
 ,nestText
 ,nestLines
 ,nestBin
) where

import Data.Bits
import Data.Word
import Data.NineP
import Data.Maybe
import Data.NineP.Bits
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Concurrent
import qualified Control.Concurrent.Forkable as F
import System.FilePath
import System.IO9.DevLayer
import System.IO9.Error
import Control.Exception
import Control.Concurrent
import System.Environment
import qualified Data.Map as M
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import System.IO9.NameSpace.Util
import System.IO9.NameSpace.Enumerator
import System.IO9.DevCons
import System.IO9.DevApps
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Control.Monad.CatchIO as C
import Data.Nesteratee


-- | Run the "init" program with the given device list and empty namespace
-- (it is expected that it builds the namespace from scratch). The parent
-- thread handle is set to this thread's handle. Standard input and output
-- are directed to the console. Console device and the builtin applications 
-- device are always initialized.

nsInit :: (MonadIO m) => AppTable m -> [DevTable] -> NameSpaceT m () -> m ()

nsInit apps dts nsi = do
  mv <- liftIO $ newMVar (M.empty)
  hu <- liftIO logName
  cons <- liftIO devCons
  apps <- liftIO $ devApps apps
  thr <- liftIO myThreadId
  attcons <- liftIO (devAttach cons Init "/" >>= flip devWalk "cons")
  let consph = PathHandle {phAttach = attcons, phCanon = "#c/cons"}
  let bids = [cons, apps]
      dts' = dts ++ bids
  let dvm = M.fromList $ zip (map devchar dts') dts'
      env = NsEnv {
        hown = hu
       ,priv = Init
       ,kdtbl = dvm
       ,nspace = mv
       ,stdinp = consph
       ,stdoutp = consph
       ,parent = thr
      }
  runNameSpaceT nsi `runReaderT` env

-- | Fork a new thread. This is the "privileged" part of running an application: actions
-- allowed at the user level are handled in the 'System.IO9.Application' module,
-- such as building an application descriptor, adjusting redirected path handles, etc.
-- This function sets the relevant parts of the application context and either
-- forks a thread for a new application or continues running in the original thread
-- (the latter is only allowed if running with the 'Init' privileges, and is not to be
-- generally used).
--
-- This function takes an 'AppDescr' (application descriptor) data structure, and
-- runs the supplied application function with type @Monad m => NameSpaceT m NineError@.
-- Note that this is not yet application itself which is supposed to be a 'Nesteratee'
-- in order to access the file I/O. The user supplied code is expected to handle that itself.
--
-- The following rules apply:
--
--  - Jumping to an application is only allowed if the current process privilege level is 'Init'.
--
--  - If the application descriptor requests certain privilege level, it can only be same
--    or lower than the parent thread has. Default ('appPriv' = 'Nothing') corresponds to
--    'HostOwner' privileges; 'Admin' or 'Init' or 'None' must be requested explicitly.
--    'World' cannot be requested: this privilege level can appear only in the 'DevAttach'
--    structure.
--
--  - If the application descriptor requests the namespace to be shared, the requested
--    privilege level should be the same as the parent thread has.
--
--  - If the 'AppJump' mode is requested, new thread is not created, and the value returned
--    is an 'AppHandle' representing a completed thread. Namespace may only be shared (in fact,
--    the current thread's environment is just reused).
--
--  If any of the above checks fails, 'nsFork' throws an exception. Execution of the new thread
--  does not even start in such case.

nsFork :: (MonadIO m, F.ForkableMonad m, C.MonadCatchIO m)
       => AppDescr                               -- ^ Application descriptor
       -> NameSpaceT m NineError                 -- ^ To run in the forked thread
       -> NameSpaceT m AppHandle                 -- ^ Returned value

nsFork ad thr = do
  let runerr = NameSpaceT . liftIO . throwIO . Located "nsFork"
  env <- NameSpaceT $ ask
  let ppriv = priv env
  ptid <- NameSpaceT $ liftIO myThreadId
  case appMode ad of
    AppJump -> do
      when (ppriv < Init) $ runerr Eperm
      thr >>= return . AppCompleted
    AppFork -> do
      let epriv = case appPriv ad of
            Nothing -> HostOwner
            Just p -> p
          world (World _ _) = True
          world _ = False
          nshare NsShare = True
          nshare _ = False
      when (world epriv) $ runerr Ebadarg
      when (epriv > ppriv) $ runerr Eperm
      when (epriv /= ppriv && nshare (appNsAdjust ad)) $ runerr Ebadarg
      let redir x y = case x of
            Nothing -> return y
            Just p -> nsEval p
      appinph <- nsStdIn >>= redir (appStdIn ad)
      appoutph <- nsStdOut >>= redir (appStdOut ad)
      NameSpaceT $ do
        mvwait <- liftIO $ newEmptyMVar
        child <- F.forkIO $ lift $ do
          newns <- liftIO $ case appNsAdjust ad of
            NsBuild _ -> newMVar (M.empty)
            NsShare -> return $ nspace env
            NsClone -> do
              nmv <- newEmptyMVar
              omv <- readMVar (nspace env)
              putMVar nmv omv
              return nmv
          let newenv = env {
                priv = epriv
               ,nspace = newns
               ,parent = ptid
               ,stdinp = appinph
               ,stdoutp = appoutph
              }
          (runNameSpaceT thr `runReaderT` newenv >>= liftIO . putMVar mvwait) `C.catches`
            [ C.Handler (liftIO . putMVar mvwait . Located "unhandled")
             ,C.Handler (\(e :: SomeException) -> liftIO $ putMVar mvwait $ OtherError $ show e)]
        return $ AppRunning child mvwait

-- | Given an 'AppHandle', wait/check for application thread completion.

nsWait :: MonadIO m
       => Bool                        -- ^ 'True' to wait, 'False' otherwise
       -> AppHandle                   -- ^ Handle of the application thread
       -> NameSpaceT m NineError      -- ^ Process status ('StillRunning' or error value)

nsWait _ (AppCompleted e) = return e

nsWait w (AppRunning _ v) = NameSpaceT $ liftIO $ case w of
  True -> takeMVar v
  False -> tryTakeMVar v >>= return . fromMaybe StillRunning

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
  NameSpaceT $ liftIO $ throwIO $ Located new Efilename


nsBind fl new old | old == "/" && isDevice new = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  pv <- asks priv
  liftIO $ withNameSpace mv $ \ns -> case M.null ns of
    True -> do
      let newnorm = normalise new
      attnew <- attdev newnorm `runReaderT` (dtb, ns, pv)
      let phnew = PathHandle {phAttach = attnew, phCanon = newnorm}
          ud = unionDir phnew
      return $ M.insert old (UnionPoint ud new) ns
    False -> bind_common fl new old dtb pv ns

nsBind fl new old = NameSpaceT $ do
  mv <- asks nspace
  dtb <- asks kdtbl
  pv <- asks priv
  liftIO $ withNameSpace mv $ bind_common fl new old dtb pv

-- | Evaluate a file path (absolute or device) using the current namespace. The function will try
-- to evaluate the entire path given, so for file creation, strip the last (not-existing-yet) part
-- of the path off. If successful, an attachment descriptor for the path is returned. Otherwise
-- the function fails (e. g. if a device driver returns an error message).

nsEval :: (MonadIO m) => FilePath -> NameSpaceT m PathHandle

nsEval fp | not (isAbsolute fp || isDevice fp) =
  NameSpaceT $ liftIO $ throw $ Located fp Efilename

nsEval fp = NameSpaceT $ do
  ns <- asks nspace >>= liftIO . readMVar
  kd <- asks kdtbl
  pv <- asks priv
  liftIO $ eval_common fp `runReaderT` (kd, ns, pv)

-- | Create a new file or directory (set 'c_DMDIR' in the @mode@ argument).
-- Creation in an union directory follows the Plan9 semantics by finding the
-- first member of the union that allows creation. The 'FilePath' supplied should not
-- contain slashes, otherwise an error will be thrown.

nsCreate :: MonadIO m
         => PathHandle                    -- ^ Handle of the directory
         -> FilePath                      -- ^ Name of the file or directory to create
         -> Word32                        -- ^ Creation mode/permissions
         -> NameSpaceT m PathHandle       -- ^ Handle of the created object

nsCreate dph fp mode | '/' `elem` fp = NameSpaceT $ liftIO $ throwIO $ Located fp Ebadarg

nsCreate dph fp mode = NameSpaceT $ do
  when (((qid_typ $ devqid $ phAttach dph) .&. c_QTDIR) == 0) $ 
    liftIO $ throwIO $ Located (show dph) Enotdir
  ns <- asks nspace >>= liftIO . readMVar
  let un = findunion (phCanon dph) ns
      dirs = filter dircr un
      dda = case dirs of
        [] -> phAttach dph
        (d:_) -> phAttach $ dirph d
  when (null dirs && not (null un)) $ liftIO $ throwIO $ Located (show dph) Enocreate
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

nsStat ph = NameSpaceT $ do
  st <- liftIO $ devStat (phAttach ph)
  u <- asks hown
  return $ mapUser u st

-- | Change some attributes of a file or directory. See <http://man.cat-v.org/plan_9/5/stat>.
-- If the 'st_name' member of the provided 'Stat' structure contains a slash, error
-- will be thrown.

nsWstat :: (MonadIO m)
        => PathHandle                     -- ^ Handle of the object whose attributes to change
        -> Stat                           -- ^ A 'Stat' structure whose fields specify changes
        -> NameSpaceT m PathHandle        -- ^ Handle of the same object with updated attrs.

nsWstat ph st | '/' `elem` st_name st = NameSpaceT $ 
  liftIO $ throwIO $ Located (st_name st) Ebadarg

nsWstat ph st = NameSpaceT $ liftIO $ do
  nda <- devWstat (phAttach ph) st
  nst <- devStat nda
  let ncn = replaceFileName (phCanon ph) (st_name nst)
  return PathHandle {
                phCanon = ncn
               ,phAttach = nda}

-- | Create a 'PathHandle' for the standard input (as set in the Namespace environment)

nsStdIn :: (MonadIO m) 
        => NameSpaceT m PathHandle

nsStdIn = NameSpaceT (asks stdinp)
  
-- | Create a 'PathHandle' for the standard output (as set in the Namespace environment)

nsStdOut :: (MonadIO m) 
         => NameSpaceT m PathHandle

nsStdOut = NameSpaceT (asks stdoutp)
  

-- | A smart constructor for an 'AppTable' making it not necessary to explicitly
-- import "Data.Map". Each application module is expected to provide its own 'AppTable'

appTable :: Monad m => [AppTable m] -> AppTable m

appTable = M.unions

-- | A smart constructor for an individual application exporting one or more entry point.
-- It expects a list of tuples where first elements are application names, and second
-- elements are entry points.

appEntry :: (Monad m) => [(FilePath, Application m)] -> AppTable m

appEntry = M.fromList
