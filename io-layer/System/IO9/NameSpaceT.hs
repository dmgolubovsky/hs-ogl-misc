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
) where

import Data.Bits
import Data.Word
import Data.NineP
import Data.NineP.Bits
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Concurrent
import System.FilePath
import System.IO9.DevLayer
import System.IO9.Error
import Control.Exception
import System.Posix.User
import qualified Data.Map as M
import System.IO9.NameSpace.Monad
import System.IO9.NameSpace.Types
import System.IO9.NameSpace.Util
import System.IO9.NameSpace.Enumerator

-- | Run the "init" program with the given device list and empty namespace
-- (it is expected that it builds the namespace from scratch).

nsInit :: (MonadIO m) => [DevTable] -> NameSpaceT m () -> m ()

nsInit dts nsi = do
  mv <- liftIO $ newMVar (M.empty)
  hu <- liftIO getLoginName
  let dvm = M.fromList $ zip (map devchar dts) dts
      env = NsEnv {
        hown = hu
       ,priv = Init
       ,kdtbl = dvm
       ,nspace = mv
      }
  runNameSpaceT nsi `runReaderT` env

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
  NameSpaceT $ liftIO $ throw Efilename

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

nsWstat ph st | '/' `elem` st_name st = NameSpaceT $ liftIO $ throwIO Ebadarg

nsWstat ph st = NameSpaceT $ liftIO $ do
  nda <- devWstat (phAttach ph) st
  nst <- devStat nda
  let ncn = replaceFileName (phCanon ph) (st_name nst)
  return PathHandle {
                phCanon = ncn
               ,phAttach = nda}


