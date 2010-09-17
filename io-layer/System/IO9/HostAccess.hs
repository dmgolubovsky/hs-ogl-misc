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

import Control.Monad
import Control.Exception
import System.IO9.DevLayer
import System.Posix.IO
import System.Posix.Files
import System.FilePath
import System.IO9.Error
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
        attach_ = haattach devtbl trmap} 
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
    


