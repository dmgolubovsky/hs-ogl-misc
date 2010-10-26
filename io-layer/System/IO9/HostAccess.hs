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

import Numeric
import Prelude hiding (catch)
import Data.Char
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
import qualified System.Posix.Directory as D
import System.FilePath
import System.Directory
import System.IO9.Error
import System.IO9.DirStream
import System.IO9.DevGen
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
       ,wstat_ = hawstat devtbl trmap
       ,create_ = hacreate devtbl trmap
       ,remove_ = haremove devtbl trmap
       ,walk_ = hawalk devtbl trmap} 
  return devtbl

-- Attach a device. Returns an attachment descriptor for the selected tree.

haattach :: DevTable -> M.Map FilePath FilePath -> ProcPriv -> FilePath -> IO DevAttach

haattach tbl tmap priv tree = do
  let mbhostfp = M.lookup tree tmap
  case mbhostfp of
    Nothing -> throwIO Ebadarg
    Just hostfp -> do
      ex <- fileExist hostfp
      when (not ex) $ throwIO Enonexist
      st <- getFileStatus hostfp
      when (not $ isDirectory st) $ throwIO Enotdir
      return DevAttach { devtbl = tbl
                        ,devpriv = priv
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
                    ,devpriv = devpriv da
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
  let xperm = stat2Mode st
      priv = devpriv da
  genPerm priv xperm flg
  case isDirectory st of
    True -> do
      when (flg /= c_OREAD) $ throwIO Ebadarg
      openDirHandle npth
    False -> do
      let iom = omode2IOMode flg
      openFile npth iom

-- Given an attachment dessriptor, return object status. The local access server
-- considers all files owned by the hostowner. Thus tildes will be placed into
-- proper fields of the returned structure.

hastat :: DevTable -> M.Map FilePath FilePath -> DevAttach -> IO Stat

hastat tbl tmap da = do
  npth <- objpath tmap da (devpath da)
  st <- getFileStatus npth
  let np = case devpath da of
              "" -> "/"
              _ -> devpath da
  rst <- stat2Stat st (head $ reverse $ splitPath np)
  return rst {st_typ = fromIntegral $ ord $ devchar tbl
             ,st_uid = "~"
             ,st_gid = "~"
             ,st_muid = "~"}

-- Given the attachment descriptor, and the new Stat structure, change
-- attributes of the object identified by the descriptor. Per 
-- <http://man.cat-v.org/plan_9/5/stat>, only the following can change:
--   * name
--   * mode (except for the DMDIR bit; DMAPPEND and DMEXCL are not
--     supported by this driver, so basically only lower 9 bits
--     can change)
--   * mtime
--   * length
--   * GID
-- New attachment descriptor will be returned.

hawstat :: DevTable -> M.Map FilePath FilePath -> DevAttach -> Stat -> IO DevAttach

hawstat tbl tmap da nst = do
  npth <- objpath tmap da (devpath da)
  st <- getFileStatus npth
  let parent = joinPath . reverse . tail . reverse $ splitPath npth
  dst <- getFileStatus parent
  let dirperm = stat2Mode dst
      priv = devpriv da
  genPerm priv dirperm c_OWRITE
  (newpath, newdp) <- case (null $ st_name nst) of
    True -> return (npth, devpath da)
    False -> do
      let newpth = replaceFileName npth (st_name nst)
          newdpth = replaceFileName (devpath da) (st_name nst)
      (rename npth newpth) `catch` (\(e :: IOError) -> throwIO Eperm)
      return (newpth, newdpth)
  nst <- getFileStatus newpath
  return DevAttach { devtbl = tbl
                    ,devpriv = devpriv da
                    ,devqid = stat2Qid nst
                    ,devpath = newdp
                    ,devtree = devtree da}
  
  


-- Create a new object (file or directory) within the directory described by an
-- attachment descriptor. Return attachment descriptor for the new object if created
-- successfully. The new object filepath must be relative and have no slashes.

hacreate :: DevTable -> M.Map FilePath FilePath -> DevAttach -> FilePath -> Word32 -> IO DevAttach

hacreate _ _ _ fp _ | isAbsolute fp || isDevice fp = throwIO Ebadarg

hacreate tbl tmap da fp newperm = do
  dpth <- objpath tmap da (devpath da)
  dst <- getFileStatus dpth
  unless (isDirectory dst) $ throwIO Enotdir
  let spp = splitPath fp
      dirperm = stat2Mode dst
      actperm = calcPerm newperm dirperm
      uperm = mode2Mode actperm
      badmask = c_DMEXCL .|. c_DMAPPEND .|. c_DMDEVICE .|. c_DMAUTH -- not supported by this driver
      priv = devpriv da
  genPerm priv dirperm c_OWRITE
  when ((actperm .&. badmask) /= 0) $ throwIO Ebadarg
  newpath <- canonicalizePath $ normalise (dpth ++ "/" ++ fp)
  unless (length spp == 1) $ throwIO Ebadarg
  nex <- fileExist newpath
  when nex $ throwIO Eexist
  case actperm .&. c_DMDIR of
    0 -> createFile newpath uperm >>= closeFd
    _ -> D.createDirectory newpath uperm
  nst <- getFileStatus newpath
  return DevAttach { devtbl = tbl
                    ,devpriv = devpriv da
                    ,devqid = stat2Qid nst
                    ,devpath = normalise (devpath da ++ "/" ++ fp)
                    ,devtree = devtree da}

-- Remove an object whose attachment descriptor is supplied. If the object is a non-
-- empty directory, operation fails as usual.

haremove :: DevTable -> M.Map FilePath FilePath -> DevAttach -> IO () 

haremove tbl tmap da = do
  dpth <- objpath tmap da (devpath da)
  isdir <- getFileStatus dpth >>= return . isDirectory
  removeLink dpth `catch` (\(e :: IOError) -> throwIO Eperm)

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
