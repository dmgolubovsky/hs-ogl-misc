------------------------------------------------------------------
-- |
-- Module      :  System.IO9.Devices.DevPosix
-- Copyright   :  (c) Dmitry Golubovsky, 2010
-- License     :  BSD-style
-- 
-- Maintainer  :  golubovsky@gmail.com
-- Stability   :  experimental
-- Portability :  GHC >= 6.12.2
-- 
--
--
-- Implementation of virtual device to access the host filesystem
------------------------------------------------------------------

module System.IO9.Devices.DevPosix (
  devPosix
) where

import Data.Word
import Data.Bits
import Data.Maybe
import System.FilePath
import System.Directory
import System.IO9.Device
import System.Posix.Files
import System.Posix.User
import Control.Concurrent
import qualified Data.Map as M

-- | Initialization of the device. The function returns a thunk holding a host
-- path that becomes a "root" of the device. If the path does not exist, the function
-- fails.

devPosix :: Bool                   -- ^ True if device connection can be shared between threads
         -> FilePath               -- ^ Host path to access via this device (preferrably
                                   -- absolute, but will be canonicalized)
         -> IO Device9P            -- ^ Function implementing the device

devPosix shr fp = do
  cfp <- canonicalizePath fp
  ex <- doesDirectoryExist cfp
  case ex of
    True -> return $ dpvers $ newdata shr cfp
    False -> return $ devError $ "Directory " ++ fp ++ "does not exist"

-- Device internal data.

data DevPosix = DevPosix {
  hfp :: FilePath                  -- root of the host file path
 ,fidmap :: M.Map Word32 FilePath  -- map of FIDs to actual paths
 ,uname :: String                  -- name of the (authenticated) user
 ,thrid :: Maybe ThreadId          -- ID of the thread that attached
 ,devshr :: Bool                   -- Device connection can be shared
}

-- Initialize device data.

newdata :: Bool -> FilePath -> DevPosix

newdata shr fp = DevPosix fp M.empty "" Nothing shr

-- Process the "Version" message, then the "Auth" message. Any other
-- sequence results in error.

-- Version: ignore the message size for now, but only proceed if the protocol version
-- is "9P2000". Any message other than "Version" results in an error, but the same
-- device connection may be reused for protocol version negotiation again.

dpvers :: DevPosix -> Device9P

dpvers devd msg@(Msg TTversion tg tv@Tversion {}) | tv_version tv == "9P2000" =
  return $ Resp9P (msg {msg_typ = TRversion}) (dpauth devd)

dpvers devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ 
                                             "Protocol negotiation failure" ++ show msg) 
                                  (dpvers devd)

-- Attach: afid is ignored. Fid and aname are written into the fidmap (aname is
-- prefixed with the stored root filepath, canonicalized, and checked to be within the root).
-- Uname as well as the current thread ID are stored in the device data.
-- Uname is currently ignored by this implementation. Note that aname may also represent 
-- a regular file. If aname points to a non-existent file or directory, the operation
-- fails, but the device remains in negotiated state.

dpauth :: DevPosix -> Device9P

dpauth devd msg@(Msg TTattach tg ta@Tattach {}) = do
  let norm = normalise (hfp devd ++ "/" ++ tat_aname ta)
  tree <- canonicalizePath norm 
  ex <- fileExist tree
  case ex && isSubdir (hfp devd) tree of
    False -> return $ Resp9P (errorMsg (msg_tag msg) $ "Invalid or non-existent path " ++ 
                                                      tat_aname ta) (dpauth devd)
    True -> do
      let fidmap' = M.insert (tat_fid ta) tree (fidmap devd)
      stat <- getFileStatus tree
      let qid = stat2qid stat
      tid <- myThreadId
      return $ Resp9P (Msg TRattach tg (Rattach qid)) 
                      (dpacc devd { 
                         fidmap = fidmap'
                        ,uname = tat_uname ta
                        ,thrid = Just tid})

-- Any message other than Auth throws a unnegotiated device entry to the client.

dpauth devd msg = return $ Resp9P (errorMsg (msg_tag msg) $ "Not authenticated") 
                                  (dpvers $ newdata (devshr devd) (hfp devd))

-- At this point we can accept walk/open/read/write/etc. messages. If the "shared" flag
-- is True, thread ID is not checked, and any thread may reuse the authenticated connection.
-- If it is False, and a request comes from the thread other authenticated thread, a fresh
-- unnegotiated device is returned. Messages like Version and Auth are not accepted at this point.
-- They will return a unnegotiated or unauthenticated device respectively.
-- The Flush message has no action as all operations are synchronous.

dpacc :: DevPosix -> Device9P

dpacc devd msg | msg_typ msg == TTversion = dpvers (newdata (devshr devd) (hfp devd)) msg

dpacc devd msg | msg_typ msg == TTauth = dpauth (newdata (devshr devd) (hfp devd)) msg
 
dpacc devd msg | msg_typ msg == TTflush = 
  return $ Resp9P (msg {msg_typ = TRflush, msg_body = Rflush}) (dpacc devd) 

dpacc devd msg = do
  let emsg x = return $ Resp9P (errorMsg (msg_tag msg) x) (dpacc devd)
  tid <- myThreadId
  if (devshr devd) && isJust (thrid devd) && (tid /= fromJust (thrid devd)) 
    then dpvers (newdata (devshr devd) (hfp devd)) msg
    else case msg_typ msg of
      TTstat -> do                  -- if file/dir does not exist, return an empty list of stats
        let sfid = ts_fid $ msg_body msg
            spath = M.lookup sfid $ fidmap devd
        case spath of
          Nothing -> emsg $ "Incorrect fid " ++ show sfid
          Just sfp -> do
            ex <- fileExist sfp     -- file could have disappeared
            case ex of
              False -> return $ Resp9P (msg {msg_typ = TRstat, msg_body = Rstat []}) (dpacc devd)
              True -> do
                st <- getFileStatus sfp
                funame <- getUserEntryForID (fileOwner st) >>= return . userName
                fgroup <- getGroupEntryForID (fileGroup st) >>= return . groupName
                let qid = stat2qid st
                    mode = stat2mode st
                    fname = if normalise (sfp ++ "/") == normalise (hfp devd ++ "/") 
                              then "/"
                              else head $ reverse $ splitPath sfp
                    ret = Stat {
                      st_typ = 0    -- these are not filled in by the driver, but
                     ,st_dev = 0    -- rather by the surrounding framework
                     ,st_qid = qid
                     ,st_mode = mode
                     ,st_atime = round $ realToFrac $ accessTime st
                     ,st_mtime = round $ realToFrac $ modificationTime st
                     ,st_length = fromIntegral $ fileSize st
                     ,st_name = fname
                     ,st_uid = funame
                     ,st_gid = fgroup
                     ,st_muid = funame
                    }
                return $ Resp9P (msg {msg_typ = TRstat, msg_body = Rstat [ret]}) (dpacc devd)
      _ -> emsg $ "Incorrect message " ++ show msg

    

-- Check that the path2 is a subdirectory of path1 (or equal to path1).

isSubdir p1 p2 | equalFilePath p1 p2 = True

isSubdir p1 p2 =
  let sp1 = splitPath p1
      sp2 = splitPath p2
  in  issub p1 p2

issub [] _ = True

issub (p1:p1s) (p2:p2s) = p1 == p2 && issub p1s p2s

-- Build a Qid from file status.

stat2qid :: FileStatus -> Qid

stat2qid stat =
  let isdir = isDirectory stat
      inode = fileID stat
      ctime = modificationTime stat
      qid = Qid {
        qid_typ = if isdir then fromIntegral c_QTDIR else 0
       ,qid_vers = round(realToFrac ctime)
       ,qid_path = fromIntegral inode
      }
  in  qid

-- Build a filemode mask in terms of the 9P definition.

stat2mode :: FileStatus -> Word32

stat2mode st =
  let umode = fileMode st
      oshift = 6
      gshift = 3
      wshift = 0
      permmap = [(ownerReadMode, c_DMREAD `shiftL` oshift)
                ,(ownerWriteMode, c_DMWRITE `shiftL` oshift)
                ,(ownerExecuteMode, c_DMEXEC `shiftL` oshift)
                ,(groupReadMode, c_DMREAD `shiftL` gshift)
                ,(groupWriteMode, c_DMWRITE `shiftL` gshift)
                ,(groupExecuteMode, c_DMEXEC `shiftL` gshift)
                ,(otherReadMode, c_DMREAD `shiftL` wshift)
                ,(otherWriteMode, c_DMWRITE `shiftL` wshift)
                ,(otherExecuteMode, c_DMEXEC `shiftL` wshift)
                ,(directoryMode, c_DMDIR)]
      nmode = foldl mbit 0 permmap
      mbit acc (umb, nmb) = case umb .&. umode of
        0 -> acc
        _ -> acc .|. nmb
  in  fromIntegral nmode



