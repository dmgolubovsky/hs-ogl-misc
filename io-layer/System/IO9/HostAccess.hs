------------------------------------------------------------------
-- |
-- Module      :  System.IO9.HostAccess
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- A virtual device to access a subtree of the host filesystem
------------------------------------------------------------------

module System.IO9.HostAccess (
  devHost
) where

import Prelude hiding (catch)
import Data.Bits
import Data.Word
import Data.List
import Control.Monad
import Control.Exception
import Data.NineP
import Data.NineP.Bits
import Data.NineP.Posix
import System.IO9.DevLayer
import System.IO
import System.Posix.IO
import System.Posix.Files
import System.FilePath
import System.Directory
import System.IO9.Error
import System.IO9.DirStream
import qualified Data.Map as M

-- | Create a device table using a map of tree names to host filesystem trees.
-- Throws an error if any of host file paths supplied does not exist or is not
-- a directory. The tree names should not contain slashes unless it is a /, and
-- should not be empty.

devHost :: [(FilePath, FilePath)] -> IO DevTable

devHost trees = do
  trs <- forM trees $ \(hostfp, tree) -> do
    ex <- fileExist hostfp
    when (not ex) $ throwIO Enonexist
    st <- getFileStatus hostfp
    when (not $ isDirectory st) $ throwIO Enotdir
    let tree' = splitPath tree
    case tree' of
      [t] -> return (t, hostfp)
      _ -> throwIO Efilename
  let trmap = M.fromList trs
      devtbl = (defDevTable 'Z') {
        attach_ = haattach devtbl trmap
       ,open_ = haopen devtbl trmap
       ,stat_ = hastat devtbl trmap
       ,walk_ = hawalk devtbl trmap} 
  return devtbl

-- Attach a device. Returns an attachment descriptor for the selected tree.

haattach :: DevTable -> M.Map FilePath FilePath -> FilePath -> IO DevAttach

haattach tbl tmap tree = do
  let mbhostfp = M.lookup tree tmap
  case mbhostfp of
    Nothing -> throwIO Ebadarg
    Just hostfp -> do
      ex <- fileExist hostfp
      when (not ex) $ throwIO Enonexist
      st <- getFileStatus hostfp
      when (not $ isDirectory st) $ throwIO Enotdir
      return DevAttach { devtbl = tbl
                        ,devqid = stat2Qid st
                        ,devpath = "/"
                        ,devtree = tree}
    
-- Walk one level to the object with given name. Returns a new attachment descriptor
-- for the new object if it exists (throws an error otherwise). Walk to dotdot always
-- drops one level from the source filepath (or remains at root).

hawalk :: DevTable -> M.Map FilePath FilePath -> DevAttach -> FilePath -> IO DevAttach

hawalk tbl tmap da "../" = walk' tbl tmap da (dropFileName $ devpath da)

hawalk tbl tmap da ".." = walk' tbl tmap da (dropFileName $ devpath da)

hawalk tbl tmap da fp = walk' tbl tmap da (devpath da </> fp)

walk' tbl tmap da fp = do
  npth <- objpath tmap da fp
  st <- getFileStatus npth
  return DevAttach { devtbl = tbl
                    ,devqid = stat2Qid st
                    ,devpath = normalise fp
                    ,devtree = devtree da}

-- Open a Handle existing object identified by an attachment descriptor.
-- This function receives a 9P2000 open flags rather than Posix open flags.
-- c_ORCLOSE is invalid. c_OCEXEC is ignored. Directories can only be opened
-- with c_OREAD.

haopen :: DevTable -> M.Map FilePath FilePath -> DevAttach -> Word8 -> IO Handle

haopen tbl tmap da flg = do
  when (flg .&. c_ORCLOSE /= 0) (throwIO Ebadarg)
  npth <- objpath tmap da (devpath da)
  st <- getFileStatus npth
  case isDirectory st of
    True -> do
      when (flg /= c_OREAD) $ throwIO Ebadarg
      openDirHandle npth
    False -> do
      let iom = omode2IOMode flg
      openFile npth iom

-- Given an attachment dessriptor, return object status.  

hastat :: DevTable -> M.Map FilePath FilePath -> DevAttach -> IO Stat

hastat tbl tmap da = do
  npth <- objpath tmap da (devpath da)
  st <- getFileStatus npth
  let np = case devpath da of
              "" -> "/"
              _ -> devpath da
  stat2Stat st (head $ reverse $ splitPath np)

-- Given an attachment descriptor, produce the host file path to the object described.

objpath :: M.Map FilePath FilePath -> DevAttach -> FilePath -> IO FilePath

objpath tmap da fp = do
  let mbhostfp = M.lookup (devtree da) tmap
  case mbhostfp of
    Nothing -> throwIO Ebadarg
    Just hostfp -> do
      npth <- canonicalizePath $ normalise (hostfp ++ "/" ++ fp)
      let npth' = if hostfp `isPrefixOf` npth
                    then npth
                    else hostfp
      ex <- (fileExist npth') `catch` (\(e :: IOError) -> return False)
      when (not ex) $ throwIO Enonexist
      return npth'
